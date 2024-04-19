import prismadb from "@/src/lib/prismadb";
import { rateLimitRequest } from "@/src/lib/rate-limit";
import { Prisma } from "@prisma/client";
import { NextRequest, NextResponse } from "next/server";

export async function POST(req: NextRequest) {
  const { name, email, company } = await req.json();

  const { success } = await rateLimitRequest(req);

  if (!success) {
    return new NextResponse("Rate limit exceeded", { status: 429 });
  }

  try {
    const entry = await prismadb.waitList.create({
      data: {
        name,
        email,
        company,
      },
    });
    return new NextResponse(JSON.stringify(entry), { status: 200 });
  } catch (error) {
    if (error instanceof Prisma.PrismaClientKnownRequestError) {
      // P2022: Unique constraint failed
      if (error.code === "P2002") {
        return new NextResponse("Already registered.", { status: 400 });
      }
    }
    console.error(error);
    return new NextResponse("Internal Error", { status: 500 });
  }
}
