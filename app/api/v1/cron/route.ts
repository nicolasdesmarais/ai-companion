import dataSourceService from "@/src/domain/services/DataSourceService";
import { NextResponse } from "next/server";

export async function GET(req: Request) {
  console.log("Cron function");

  await dataSourceService.pollDataSourceStatus();

  return NextResponse.json({ message: "Cron function" });
}
