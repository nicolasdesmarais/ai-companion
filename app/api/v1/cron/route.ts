import { NextResponse } from "next/server";

export const runtime = "edge";

export async function GET() {
  console.log("Cron function");

  return NextResponse.json({ message: "Cron function" });
}
