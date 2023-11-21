import prismadb from "@/src/lib/prismadb";
import { auth } from "@clerk/nextjs";
import { NextResponse } from "next/server";

export async function GET(req: Request) {
  try {
    const { userId, sessionClaims } = await auth();
    if (!userId || !(sessionClaims?.meta as any)?.superuser) {
      return new NextResponse("Unauthorized", { status: 401 });
    }
    const datasources = await prismadb.dataSource.findMany({
      orderBy: [
        {
          updatedAt: "desc",
        },
      ],
      include: {
        knowledges: {
          include: {
            knowledge: true,
          },
        },
        ais: {
          include: {
            ai: true,
          },
        },
      },
    });
    return NextResponse.json(datasources);
  } catch (error) {
    console.error("[GET v1/super/data-sources]", error);
    return new NextResponse("Internal Error", { status: 500 });
  }
}
