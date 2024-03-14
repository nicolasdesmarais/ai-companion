import { FileStorageService } from "@/src/domain/services/FileStorageService";
import { htmlToMarkdown } from "@/src/lib/htmlUtils";
import { ActorRun, ActorStartOptions, ApifyClient } from "apify-client";

const client = new ApifyClient({
  token: process.env.APIFY_TOKEN,
});

const webScraperActorId = process.env.APIFY_CHEERIO_SCRAPER_ACTOR_ID;
const runMode = process.env.APIFY_RUN_MODE;
const webhookUrl = process.env.APIFY_WEBHOOK_URL;
const webhookSecret = process.env.APIFY_WEBHOOK_SECRET;

const failedStatuses = ["FAILED", "ABORTING", "ABORTED"];
const partialStatuses = ["TIMING-OUT", "TIMED-OUT"];
const succeededStatus = ["SUCCEEDED"];

export interface ActorRunResult {
  status: ActorRunStatus;
  items: ActorRunItem[];
}

export interface ActorRunItem {
  url: string;
  contentBlobUrl: string;
  filename: string;
  mimeType: string;
}

export enum ActorRunStatus {
  INDEXING = "INDEXING",
  PARTIALLY_COMPLETED = "PARTIALLY_COMPLETED",
  COMPLETED = "COMPLETED",
  FAILED = "FAILED",
}

export class ApifyAdapter {
  async startUrlIndexing(
    orgId: string,
    dataSourceId: string,
    knowledgeId: string,
    url: string
  ) {
    if (!webScraperActorId) {
      throw new Error("APIFY_WEB_SCRAPER_ACTOR_ID is not set");
    }

    if (!url) {
      return;
    }

    const actorRun = await client
      .actor(webScraperActorId)
      .start(
        this.getWebScraperInput(url),
        this.getActorStartOptions(orgId, dataSourceId, knowledgeId, url)
      );

    return actorRun.id;
  }

  private getActorStartOptions(
    orgId: string,
    dataSourceId: string,
    knowledgeId: string,
    url: string
  ): ActorStartOptions {
    return {
      timeout: this.getActorTimeout(),
      memory: this.getActorMemory(),
      webhooks: [
        {
          eventTypes: [
            "ACTOR.RUN.SUCCEEDED",
            "ACTOR.RUN.FAILED",
            "ACTOR.RUN.ABORTED",
            "ACTOR.RUN.TIMED_OUT",
          ],
          requestUrl: webhookUrl,
          headersTemplate: `{
            "X-Apify-Webhook-Secret": "${webhookSecret}"
          }`,
          payloadTemplate: `{
            "eventType": {{eventType}},
            "eventData": {{eventData}},
            "orgId" : "${orgId}",
            "dataSourceId": "${dataSourceId}",
            "knowledgeId": "${knowledgeId}",
            "rootUrl": "${url}"
        }`,
        },
      ],
    };
  }

  private getWebScraperInput(url: string) {
    return {
      runMode: runMode,
      startUrls: [
        {
          url: url,
        },
      ],
      keepUrlFragments: true,
      excludes: [
        {
          glob: "/**/*.{png,jpg,jpeg,pdf}",
        },
      ],
      pageFunction:
        // For a complete list of its properties and functions,
        // see https://apify.com/apify/web-scraper#page-function
        async function pageFunction(context: any) {
          const { $ } = context;
          const url = context.request.url;
          const html = $.html();

          const originalUrlObj = new URL(url);

          // Extract the base path without the query or hash
          let basePath = originalUrlObj.href.substring(
            0,
            originalUrlObj.href.lastIndexOf("/") + 1
          );

          const hrefs = $("a[href]")
            .map((_: any, el: any) => $(el).attr("href"))
            .get();

          for (const href of hrefs) {
            if (href && !href.startsWith("#")) {
              // Exclude anchor URLs
              let resolvedUrl;
              if (href.startsWith("/")) {
                // Root-relative URL
                resolvedUrl = `${originalUrlObj.protocol}//${originalUrlObj.host}${href}`;
              } else {
                // Document-relative URL, resolve against the basePath
                resolvedUrl = new URL(
                  href,
                  `${originalUrlObj.origin}${basePath}`
                ).href;
              }

              const resolvedUrlObj = new URL(resolvedUrl);

              // Ensure same base path and exclude the exact same URL
              if (
                resolvedUrlObj.href.startsWith(basePath) &&
                resolvedUrlObj.href !== url
              ) {
                context.enqueueRequest({ url: resolvedUrlObj.href });
              }
            }
          }

          return {
            url,
            html,
          };
        },
      injectJQuery: true,
      proxyConfiguration: {
        useApifyProxy: true,
      },
      proxyRotation: "RECOMMENDED",
      maxRequestRetries: 2,
      maxPagesPerCrawl: this.getMaxPagesPerCrawl(),
      maxResultsPerCrawl: 0,
      maxCrawlingDepth: 0,
      maxConcurrency: 50,
      pageLoadTimeoutSecs: 15,
      pageFunctionTimeoutSecs: 15,
      closeCookieModals: true,
      maxScrollHeightPixels: 5000,
      ignoreSslErrors: true,
    };
  }

  public async getActorRunBatch(
    actorRunId: string,
    rootUrl: string,
    offset: number,
    limit: number
  ): Promise<ActorRunResult> {
    const actorRun = await client.run(actorRunId).get();
    if (!actorRun) {
      return {
        status: ActorRunStatus.INDEXING,
        items: [],
      };
    }

    const status = this.getActorRunStatus(actorRun);
    const dataset = await client.run(actorRunId).dataset();
    const listItems = await dataset.listItems({
      offset,
      limit,
    });

    const items: ActorRunItem[] = [];
    for (const item of listItems.items) {
      const { url, html } = item;
      if (!url || !html) {
        continue;
      }
      const urlString = url as string;
      const markdown = htmlToMarkdown(item.html as string);
      const filename = `${urlString}.md`;
      const contentBlobUrl = await FileStorageService.put(filename, markdown);

      items.push({
        url: urlString,
        contentBlobUrl,
        filename,
        mimeType: "text/markdown",
      });
    }

    return {
      status,
      items,
    };
  }

  private getActorRunStatus(actorRun: ActorRun) {
    if (failedStatuses.includes(actorRun.status)) {
      return ActorRunStatus.FAILED;
    }
    if (partialStatuses.includes(actorRun.status)) {
      return ActorRunStatus.PARTIALLY_COMPLETED;
    }
    if (succeededStatus.includes(actorRun.status)) {
      return ActorRunStatus.COMPLETED;
    }
    return ActorRunStatus.INDEXING;
  }

  private getMaxPagesPerCrawl(): number {
    const maxPagesPerCrawl = process.env.APIFY_MAX_PAGES_PER_CRAWL;
    if (maxPagesPerCrawl) {
      const maxPagesPerCrawlNumber = parseInt(maxPagesPerCrawl);
      if (!isNaN(maxPagesPerCrawlNumber)) {
        return maxPagesPerCrawlNumber;
      }
    }
    return 0;
  }

  private getActorTimeout(): number | undefined {
    const actorTimeout = process.env.APIFY_ACTOR_TIMEOUT;
    if (actorTimeout) {
      const actorTimeoutNumber = parseInt(actorTimeout);
      if (!isNaN(actorTimeoutNumber)) {
        return actorTimeoutNumber;
      }
    }
  }

  private getActorMemory(): number | undefined {
    const actorMemory = process.env.APIFY_ACTOR_MEMORY;
    if (actorMemory) {
      const actorMemoryNumber = parseInt(actorMemory);
      if (!isNaN(actorMemoryNumber)) {
        return actorMemoryNumber;
      }
    }
  }
}

const apifyAdapter = new ApifyAdapter();
export default apifyAdapter;
