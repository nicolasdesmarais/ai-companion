import { ActorStartOptions, ApifyClient } from "apify-client";
import { KnowledgeIndexingResultStatus } from "../types/KnowlegeIndexingResult";

const client = new ApifyClient({
  token: process.env.APIFY_TOKEN,
});

const webScraperActorId = process.env.APIFY_WEB_SCRAPER_ACTOR_ID;
const runMode = process.env.APIFY_RUN_MODE;
const webhookUrl = process.env.APIFY_WEBHOOK_URL;
const webhookSecret = process.env.APIFY_WEBHOOK_SECRET;

const failedStatuses = ["FAILED", "ABORTING", "ABORTED"];
const partialStatuses = ["TIMING-OUT", "TIMED-OUT"];

export class ApifyWebScraper {
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
        this.getActorStartOptions(orgId, dataSourceId, knowledgeId)
      );

    return actorRun.id;
  }

  private getActorStartOptions(
    orgId: string,
    dataSourceId: string,
    knowledgeId: string
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
            "knowledgeId": "${knowledgeId}"
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
      linkSelector: "a[href]",
      globs: [
        {
          glob: `${url}/**/*`,
        },
      ],
      excludes: [
        {
          glob: "/**/*.{png,jpg,jpeg,pdf}",
        },
      ],
      pageFunction:
        // For a complete list of its properties and functions,
        // see https://apify.com/apify/web-scraper#page-function
        async function pageFunction(context: any) {
          const $ = context.jQuery;
          const pageTitle = $("title").first().text();

          // Get all text from meaningful elements
          let allText = "";
          $("h1, h2, h3, h4, h5, h6, p, a, li, span").each(
            (_: any, element: HTMLElement) => {
              allText += $(element).text() + "\n"; // Add a newline for separation
            }
          );

          // Remove extra spaces and newlines
          allText = allText.replace(/\s\s+/g, " ").trim();

          context.log.info(
            `URL: ${context.request.url}, TITLE: ${pageTitle}, allText: ${allText}`
          );

          return {
            url: context.request.url,
            pageTitle,
            allText,
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
    };
  }

  public async getActorRunResult(actorRunId: string) {
    let result: {
      status: KnowledgeIndexingResultStatus;
      items: any[];
    };

    const actorRun = await client.run(actorRunId).get();
    if (!actorRun) {
      result = {
        status: KnowledgeIndexingResultStatus.FAILED,
        items: [],
      };

      return result;
    }

    let status;
    if (failedStatuses.includes(actorRun.status)) {
      status = KnowledgeIndexingResultStatus.FAILED;
    } else if (partialStatuses.includes(actorRun.status)) {
      status = KnowledgeIndexingResultStatus.PARTIAL;
    } else {
      status = KnowledgeIndexingResultStatus.SUCCESSFUL;
    }

    const dataset = await client.run(actorRunId).dataset();
    const listItems = await dataset.listItems();
    const items = listItems.items.map((item) => {
      return {
        pageTitle: item.pageTitle,
        allText: item.allText,
      };
    });

    result = {
      status,
      items,
    };
    return result;
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

const apifyWebScraper = new ApifyWebScraper();
export default apifyWebScraper;
