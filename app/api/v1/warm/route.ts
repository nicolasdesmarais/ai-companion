import { NextRequest, NextResponse } from "next/server";

const paths = ["/", "/ai/new/edit", "/chat/"];

const fetchPath = async (origin: string, path: string) => {
  const resp = await fetch(origin + path, {
    headers: { Authorization: `Bearer ${process.env.AUTOMATION_TOKEN}` },
  });
  return {
    status: resp.status,
    path,
  };
};

export async function GET(req: NextRequest) {
  try {
    const { origin } = new URL(req.url);
    const promises = paths.map((path) => fetchPath(origin, path));
    const results = await Promise.all(promises);
    return new NextResponse(JSON.stringify(results), { status: 200 });
  } catch (error) {
    console.error(error);
    return new NextResponse("Internal Error", { status: 500 });
  }
}
