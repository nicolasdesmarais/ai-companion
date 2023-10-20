import { ApifyClient } from "apify-client";

const client = new ApifyClient({
  token: process.env.APIFY_TOKEN,
});
const webScraperActorId = process.env.APIFY_WEB_SCRAPER_ACTOR_ID;

export class ApifyService {
  async createWebUrlKnowledge(userId: string, url: string) {
    if (!webScraperActorId) {
      throw new Error("APIFY_WEB_SCRAPER_ACTOR_ID is not set");
    }

    const actorRun = await client
      .actor(webScraperActorId)
      .start(this.getWebScraperInput(url));

    console.log("Actor run starter: " + actorRun.id);
  }

  private getWebScraperInput(url: string) {
    return {
      runMode: "DEVELOPMENT",
      startUrls: [
        {
          url: url,
        },
      ],
      keepUrlFragments: false,
      linkSelector: "a[href]",
      globs: [
        {
          glob: `${url}/*/*`,
        },
      ],
      pseudoUrls: [],
      excludes: [
        {
          glob: "/**/*.{png,jpg,jpeg,pdf}",
        },
      ],
      // The function accepts a single argument: the "context" object.
      pageFunction:
        // For a complete list of its properties and functions,
        // see https://apify.com/apify/web-scraper#page-function
        async function pageFunction(context: any) {
          // This statement works as a breakpoint when you're trying to debug your code. Works only with Run mode: DEVELOPMENT!
          // debugger;

          // jQuery is handy for finding DOM elements and extracting data from them.
          // To use it, make sure to enable the "Inject jQuery" option.
          const $ = context.jQuery;
          const pageTitle = $("title").first().text();
          const h1 = $("h1").first().text();
          const first_h2 = $("h2").first().text();
          const random_text_from_the_page = $("p").first().text();

          // Print some information to actor log
          context.log.info(`URL: ${context.request.url}, TITLE: ${pageTitle}`);

          // Manually add a new page to the queue for scraping.
          await context.enqueueRequest({ url: "http://www.example.com" });

          // Return an object with the data extracted from the page.
          // It will be stored to the resulting dataset.
          return {
            url: context.request.url,
            pageTitle,
            h1,
            first_h2,
            random_text_from_the_page,
          };
        },
      injectJQuery: true,
      proxyConfiguration: {
        useApifyProxy: true,
      },
      proxyRotation: "RECOMMENDED",
      initialCookies: [],
      useChrome: false,
      headless: true,
      ignoreSslErrors: false,
      ignoreCorsAndCsp: false,
      downloadMedia: true,
      downloadCss: true,
      maxRequestRetries: 3,
      maxPagesPerCrawl: 0,
      maxResultsPerCrawl: 0,
      maxCrawlingDepth: 0,
      maxConcurrency: 50,
      pageLoadTimeoutSecs: 60,
      pageFunctionTimeoutSecs: 60,
      waitUntil: ["networkidle2"],
      preNavigationHooks: `// We need to return array of (possibly async) functions here.
        // The functions accept two arguments: the "crawlingContext" object
        // and "gotoOptions".
        [
            async (crawlingContext, gotoOptions) => {
                // ...
            },
        ]`,
      postNavigationHooks: `// We need to return array of (possibly async) functions here.
        // The functions accept a single argument: the "crawlingContext" object.
        [
            async (crawlingContext) => {
                // ...
            },
        ]`,
      breakpointLocation: "NONE",
      closeCookieModals: false,
      maxScrollHeightPixels: 5000,
      debugLog: false,
      browserLog: false,
      customData: {},
    };
  }
}
