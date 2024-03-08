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
    const originalUrlObj = new URL(baseUrl);
    const originalDomain = originalUrlObj.hostname;

    const $ = cheerio.load(html);
    const urls: string[] = [];
    $("a").each((i, link) => {
      const href = $(link).attr("href");
      if (href) {
        const hrefObj = new URL(href, originalUrlObj.origin); // Resolve relative URLs
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
