import { ApifyClient } from "apify-client";

const client = new ApifyClient({
  token: process.env.APIFY_TOKEN,
});
const webScraperActorId = process.env.APIFY_WEB_SCRAPER_ACTOR_ID;
const runMode = process.env.APIFY_RUN_MODE;

export class ApifyService {
  async createWebUrlKnowledge(userId: string, url: string) {
    if (!webScraperActorId) {
      throw new Error("APIFY_WEB_SCRAPER_ACTOR_ID is not set");
    }

    if (!url) {
      return;
    }

    const actorRun = await client
      .actor(webScraperActorId)
      .start(this.getWebScraperInput(url));

    console.log("Actor run started: " + actorRun.id);
  }

  private getWebScraperInput(url: string) {
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
          $("p, h1, h2, h3, h4, h5, h6").each(
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
      maxRequestRetries: 3,
      maxPagesPerCrawl: 0,
      maxResultsPerCrawl: 0,
      maxCrawlingDepth: 0,
      maxConcurrency: 50,
      pageLoadTimeoutSecs: 60,
      pageFunctionTimeoutSecs: 60,
      closeCookieModals: true,
      maxScrollHeightPixels: 5000,
    };
  }
}
