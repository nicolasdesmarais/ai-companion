import { withErrorHandler } from "@/src/middleware/ErrorMiddleware";
import { NextRequest, NextResponse } from "next/server";

export const maxDuration = 300;

const testAiId = process.env.AUTOMATION_AI_ID;

const paths = [
  "/",
  "/ai/new/edit",
  "/chat/",
  "/api/v1/me/chats",
  "/api/v1/me/groups",
  "/api/v1/categories",
  `/ai/${testAiId}`,
  `/api/v1/ai/${testAiId}/rating/all`,
  `/api/v1/ai/${testAiId}/data-sources`,
];

const fetchPath = async (origin: string, path: string) => {
  const url = origin + path;
  const resp = await fetch(url, {
    headers: { Authorization: `Bearer ${process.env.AUTOMATION_TOKEN}` },
  });
  return {
    status: resp.status,
    url,
  };
};

async function getHandler(req: NextRequest) {
  const { origin } = new URL(req.url);
  const promises = paths.map((path) => fetchPath(origin, path));
  const results = await Promise.all(promises);
  return new NextResponse(JSON.stringify(results), { status: 200 });
}

export const GET = withErrorHandler(getHandler);
