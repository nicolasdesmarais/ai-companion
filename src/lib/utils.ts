import { type ClassValue, clsx } from "clsx";
import { twMerge } from "tailwind-merge";

export function cn(...inputs: ClassValue[]) {
  return twMerge(clsx(inputs));
}

export function absoluteUrl(path: string) {
  return `${process.env.NEXT_PUBLIC_APP_URL}${path}`;
}

export const delay = (ms: number | undefined) =>
  new Promise((res) => setTimeout(res, ms));

export function getCurrentDateStr() {
  const now = new Date();
  const dayOfWeek = now.toLocaleString(window.navigator.language, {
    weekday: "long",
  });
  return `${dayOfWeek}, ${now.toLocaleString()}`;
}
