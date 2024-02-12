import { getEncoding } from "js-tiktoken";

const encoding = getEncoding("cl100k_base");

export const getTokenLength = (value: string) => encoding.encode(value).length;

export const convertGigabytesToTokens = (gigabytes: number) => {
  const bytes = gigabytes * 1024 * 1024 * 1024;
  const tokens = bytes / 4;
  return tokens;
};

export const convertTokensToGigabytes = (tokens: number) => {
  const bytes = tokens * 4;
  const gigabytes = bytes / (1024 * 1024 * 1024);
  return gigabytes;
};
