import { type ClassValue, clsx } from "clsx";
import { twMerge } from "tailwind-merge";

export function cn(...inputs: ClassValue[]) {
  return twMerge(clsx(inputs));
}

export function absoluteUrl(path: string) {
  return `${process.env.NEXT_PUBLIC_APP_URL}${path}`;
}

const superusers = [
  "user_2WXwDNSh5x5zxnabNzpSN2Z1JWs",
  "user_2Wr3B1xXEqC2rZaa9uvzG61sFys",
];
export function isSuperuser(userId: string) {
  return superusers.includes(userId);
}
