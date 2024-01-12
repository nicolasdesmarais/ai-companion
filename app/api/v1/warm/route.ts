import { withErrorHandler } from "@/src/middleware/ErrorMiddleware";
import { NextRequest, NextResponse } from "next/server";

export const maxDuration = 300;

const paths = ["/", "/ai/new/edit", "/chat/"];

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
