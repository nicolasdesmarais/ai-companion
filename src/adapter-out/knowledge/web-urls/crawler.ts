import axios from "axios";
import * as cheerio from "cheerio";

import { NodeHtmlMarkdown } from "node-html-markdown";

interface Page {
  url: string;
  content: string;
  childUrls: string[];
}

export class Crawler {
  public async crawl(url: string): Promise<Page> {
    const html = await this.fetchPage(url);

    const content = this.parseHtml(html);

    const childUrls = this.extractChildUrls(html, url);

    return { url, content, childUrls };
  }

  private async fetchPage(url: string): Promise<string> {
    try {
      const { data } = await axios.get(url);
      return data;
    } catch (error) {
      throw new Error("Error fetching HTML content", error);
    }
  }

  private parseHtml(html: string): string {
    const $ = cheerio.load(html);
    $("a").removeAttr("href");
    return NodeHtmlMarkdown.translate($.html());
  }

  private extractChildUrls(html: string, baseUrl: string): string[] {
    const $ = cheerio.load(html);
    const originalUrlObj = new URL(baseUrl);
    const originalDomain = originalUrlObj.hostname;

    // Correctly resolve the base path for relative URLs
    let basePath = originalUrlObj.href.substring(
      0,
      originalUrlObj.href.lastIndexOf("/") + 1
    );

    const urls: string[] = [];
    $("a").each((i, link) => {
      const href = $(link).attr("href");
      if (href) {
        let resolvedUrl;
        if (href.startsWith("/")) {
          // Root-relative URL
          resolvedUrl = `${originalUrlObj.protocol}//${originalUrlObj.host}${href}`;
        } else {
          // Document-relative URL, resolve against the basePath
          resolvedUrl = new URL(href, basePath).href;
        }

        const hrefObj = new URL(resolvedUrl);
        if (hrefObj.hostname === originalDomain) {
          urls.push(hrefObj.href);
        }
      }
    });

    return urls;
  }
}

const crawler = new Crawler();
export default crawler;

export type { Page };
