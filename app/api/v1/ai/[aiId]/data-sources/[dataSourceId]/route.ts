import dataSourceService from "@/src/domain/services/DataSourceService";
import { auth } from "@clerk/nextjs";
import { NextResponse } from "next/server";

export async function DELETE(
  request: Request,
  { params }: { params: { aiId: string; dataSourceId: string } }
) {
  try {
    const authentication = await auth();
    const orgId = authentication?.orgId;
    const userId = authentication?.userId;

    if (!userId || !orgId) {
      return new NextResponse("Unauthorized", { status: 401 });
    }

    await dataSourceService.deleteDataSource(params.aiId, params.dataSourceId);
    return new NextResponse(null, { status: 204 });
  } catch (error) {
    console.log("[KNOWLEDGE_DELETE]", error);
    return new NextResponse("Internal Error", { status: 500 });
  }
}
