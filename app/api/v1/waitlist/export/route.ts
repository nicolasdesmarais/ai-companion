import { NextRequest, NextResponse } from "next/server";
import { rateLimitRequest } from "@/src/lib/rate-limit";
import prismadb from "@/src/lib/prismadb";
import { Prisma } from "@prisma/client";

export async function GET(req: NextRequest) {
  const { searchParams } = new URL(req.url);

  const key = searchParams.get("key");

  if (key !== process.env.WAITLIST_KEY) {
    return new NextResponse("Unauthorized", { status: 401 });
  }

  try {
    const list = await prismadb.waitList.findMany();
    return new NextResponse(JSON.stringify(list), { status: 200 });
  } catch (error) {
    console.error(error);
    return new NextResponse("Internal Error", { status: 500 });
  }
}
