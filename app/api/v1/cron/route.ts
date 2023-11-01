import dataSourceService from "@/src/domain/services/DataSourceService";
import { NextResponse } from "next/server";

export const runtime = "edge";

export async function GET(req: Request) {
  console.log("Cron function");
  console.log("Req:" + JSON.stringify(req));
  console.log("Headers:" + JSON.stringify(req.headers));

  await dataSourceService.pollDataSourceStatus();

  return NextResponse.json({ message: "Cron function" });
}
