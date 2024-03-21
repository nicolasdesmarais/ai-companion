import * as cheerio from "cheerio";

import { NodeHtmlMarkdown } from "node-html-markdown";

export function htmlToMarkdown(html: string): string {
  if (!html) {
    return "";
  }
  const $ = cheerio.load(html);
  $("a").removeAttr("href");
  $("*").removeAttr("src");
  return NodeHtmlMarkdown.translate($.html());
}
