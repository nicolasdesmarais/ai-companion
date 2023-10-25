import { ActorStartOptions, ApifyClient } from "apify-client";

const client = new ApifyClient({
  token: process.env.APIFY_TOKEN,
});
const webScraperActorId = process.env.APIFY_WEB_SCRAPER_ACTOR_ID;
const runMode = process.env.APIFY_RUN_MODE;
const webhookUrl = process.env.APIFY_WEBHOOK_URL;
const webhookSecret = process.env.APIFY_WEBHOOK_SECRET;
const maxPagesPerCrawl = process.env.APIFY_MAX_PAGES_PER_CRAWL;

export class ApifyService {
  async createWebUrlKnowledge(knowledgeId: string, url: string) {
    if (!webScraperActorId) {
      throw new Error("APIFY_WEB_SCRAPER_ACTOR_ID is not set");
    }

    if (!url) {
      return;
    }

    const actorRun = await client
      .actor(webScraperActorId)
      .start(
        this.getWebScraperInput(knowledgeId, url),
        this.getActorStartOptions(knowledgeId)
      );

    console.log("Actor run started: " + actorRun.id);
  }

  private getActorStartOptions(knowledgeId: string): ActorStartOptions {
    return {
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
            "knowledgeId": "${knowledgeId}"
        }`,
        },
      ],
    };
  }

  private getWebScraperInput(knowledgeId: string, url: string) {
    return {
      runMode: runMode,
      startUrls: [
        {
          url: url,
        },
      ],
      keepUrlFragments: false,
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
            knowledge: context.customData.knowledgeId,
          };
        },
      injectJQuery: true,
      proxyConfiguration: {
        useApifyProxy: true,
      },
      proxyRotation: "RECOMMENDED",
      maxRequestRetries: 3,
      maxPagesPerCrawl: this.getMaxPagesPerCrawl(),
      maxResultsPerCrawl: 0,
      maxCrawlingDepth: 0,
      maxConcurrency: 50,
      pageLoadTimeoutSecs: 60,
      pageFunctionTimeoutSecs: 60,
      closeCookieModals: true,
      maxScrollHeightPixels: 5000,
      customData: { knowledgeId: knowledgeId },
    };
  }

  public async getActorRunResult(actorRunId: string) {
    const dataset = await client.run(actorRunId).dataset();
    const listItems = await dataset.listItems();
    return listItems.items.map((item) => {
      return {
        pageTitle: item.pageTitle,
        allText: item.allText,
        knowledge: item.knowledge,
      };
    });
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
}
