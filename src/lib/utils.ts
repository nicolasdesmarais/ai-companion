import { clsx, type ClassValue } from "clsx";
import { twMerge } from "tailwind-merge";

export function cn(...inputs: ClassValue[]) {
  return twMerge(clsx(inputs));
}

export function absoluteUrl(path: string) {
  return `${process.env.NEXT_PUBLIC_APP_URL}${path}`;
}

export function resolveUrl(url: string): URL {
  if (!url.startsWith("http://") && !url.startsWith("https://")) {
    url = "https://" + url;
  }
  const resolvedUrl = new URL(url);
  return resolvedUrl;
}

export const delay = (ms: number | undefined) =>
  new Promise((res) => setTimeout(res, ms));

export function getCurrentDateStr() {
  const now = new Date();
  const dayOfWeek = now.toLocaleString(undefined, {
    weekday: "long",
  });
  return `${dayOfWeek}, ${now.toLocaleString()}`;
}

export const pixelCrop = (src: string | undefined, crop: string) => {
  if (src) {
    return src.replace(
      /image\/upload\/.*\//gm,
      `image/upload/c_fill,g_auto,${crop}/`
    );
  }
};

export const aspectFill = (src: string | undefined, fill: string) => {
  if (src) {
    return src.replace(
      /image\/upload\/.*\//gm,
      `image/upload/b_gen_fill,c_pad,ar_${fill}/`
    );
  }
};

export function containsMySQLSpecialChars(str: string): boolean {
  const specialCharsRegex = /[+\-><()~*@"]/;
  return specialCharsRegex.test(str);
}

export function serializeQueryParams(params: Record<string, string>) {
  return Object.entries(params)
    .map(([key, value]) => `${key}=${encodeURIComponent(value)}`)
    .join("&");
}
