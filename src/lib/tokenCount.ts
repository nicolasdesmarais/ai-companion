// @ts-expect-error
import wasm from "tiktoken/lite/tiktoken_bg.wasm?module";
import model from "tiktoken/encoders/cl100k_base.json";
import { init, Tiktoken } from "tiktoken/lite/init";

export const config = { runtime: "edge" };

export async function getTokenLength(value: string) {
  await init((imports: any) => WebAssembly.instantiate(wasm, imports));

  const encoding = new Tiktoken(
    model.bpe_ranks,
    model.special_tokens,
    model.pat_str
  );

  const tokens = encoding.encode(value);
  encoding.free();

  return tokens.length;
}

// import { getEncoding } from "js-tiktoken";

// const encoding = getEncoding("cl100k_base");

// export const getTokenLength = (value: string) =>
//   Number(encoding.encode(value).length);
