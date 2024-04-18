import { Ratelimit } from "@upstash/ratelimit";
import { Redis } from "@upstash/redis";
import { NextRequest } from "next/server";

type Unit = "ms" | "s" | "m" | "h" | "d";
type Duration = `${number} ${Unit}` | `${number}${Unit}`;

export async function rateLimit(identifier: string) {
  const ratelimit = new Ratelimit({
    redis: Redis.fromEnv(),
    limiter: Ratelimit.slidingWindow(10, "10 s"),
    analytics: true,
    prefix: "@upstash/ratelimit",
  });

  return await ratelimit.limit(identifier);
}

export async function tokenBucketRateLimit(
  identifier: string,
  refillRate: number,
  interval: Duration,
  maxTokens: number,
  requestedTokens: number
): Promise<boolean> {
  const ratelimit = new Ratelimit({
    redis: Redis.fromEnv(),
    limiter: Ratelimit.tokenBucket(refillRate, interval, maxTokens),
    analytics: true,
    prefix: "@upstash/ratelimit",
  });

  const { success } = await ratelimit.limit(identifier, {
    rate: requestedTokens,
  });
  return success;
}

export async function rateLimitRequest(req: NextRequest) {
  const ip = req.ip || req.headers.get("x-forwarded-for") || "";
  console.log("rate limit check for ip: ", ip);
  return await rateLimit(ip);
}
