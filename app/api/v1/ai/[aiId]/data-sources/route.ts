import dataSourceService from "@/src/domain/services/DataSourceService";
import { auth } from "@clerk/nextjs";
import { NextResponse } from "next/server";

export async function GET(
  request: Request,
  { params }: { params: { aiId: string } }
) {
  try {
    const authentication = await auth();
    const orgId = authentication?.orgId;
    const userId = authentication?.userId;

    if (!userId || !orgId) {
      return new NextResponse("Unauthorized", { status: 401 });
    }

    const dataSources = await dataSourceService.getDataSources(
      orgId,
      userId,
      params.aiId
    );

    return NextResponse.json(dataSources);
  } catch (error) {
    console.log("[DATASOURCE_GET]", error);
    return new NextResponse("Internal Error", { status: 500 });
  }
}
