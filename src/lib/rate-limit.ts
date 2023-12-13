import { Ratelimit } from "@upstash/ratelimit";
import { Redis } from "@upstash/redis";
import { NextRequest } from "next/server";

export async function rateLimit(identifier: string) {
  const ratelimit = new Ratelimit({
    redis: Redis.fromEnv(),
    limiter: Ratelimit.slidingWindow(10, "10 s"),
    analytics: true,
    prefix: "@upstash/ratelimit",
  });

  return await ratelimit.limit(identifier);
}

export async function rateLimitRequest(req: NextRequest) {
  const ip = req.ip || req.headers.get("x-forwarded-for") || "";
  console.log("rate limit check for ip: ", ip);
  return await rateLimit(ip);
}
