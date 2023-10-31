import { getEncoding } from "js-tiktoken";

const encoding = getEncoding("cl100k_base");

export const getTokenLength = (value: string) => encoding.encode(value).length;
