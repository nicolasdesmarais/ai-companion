import { FileStorageService } from "@/src/domain/services/FileStorageService";
import { resolveUrl } from "@/src/lib/utils";
import { ActorRun, ActorStartOptions, ApifyClient } from "apify-client";

const client = new ApifyClient({
  token: process.env.APIFY_TOKEN,
});

const actorId = process.env.APIFY_WEBSITE_CONTENT_CRAWLER_ACTOR_ID;
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

export class ApifyWebsiteContentCrawler {
  public async startUrlIndexing(
    orgId: string,
    dataSourceId: string,
    knowledgeId: string,
    url: string
  ) {
    if (!actorId) {
      throw new Error("APIFY_WEBSITE_CONTENT_CRAWLER_ACTOR_ID is not set");
    }

    if (!url) {
      return;
    }

    const actorRun = await client
      .actor(actorId)
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
    const urlObj = resolveUrl(url);
    const basePath = urlObj.href.substring(0, urlObj.href.lastIndexOf("/"));

    return {
      runMode: runMode,
      crawlerType: "cheerio",
      saveMarkdown: true,
      startUrls: [
        {
          url: urlObj.href,
        },
      ],
      includeUrlGlobs: [`${basePath}/**/*`],
      proxyConfiguration: {
        useApifyProxy: true,
      },
      proxyRotation: "RECOMMENDED",
      maxRequestRetries: 2,
      maxCrawlPages: this.getMaxPagesPerCrawl(),
      maxConcurrency: 50,
      requestTimeoutSecs: 15,
    };
  }

  public async getActorRunBatch(
    actorRunId: string,
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
      fields: ["url", "markdown"],
    });

    const items: ActorRunItem[] = [];
    for (const item of listItems.items) {
      const { url, markdown } = item;
      if (!url) {
        continue;
      }
      const urlString = url as string;
      const markdownString = markdown ? (markdown as string) : " ";
      const filename = `${urlString}.md`;
      const contentBlobUrl = await FileStorageService.put(
        filename,
        markdownString
      );

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

const apifyWebsiteContentCrawler = new ApifyWebsiteContentCrawler();
export default apifyWebsiteContentCrawler;
