import { NextRequest } from "next/server";

export const config = {
  runtime: "edge",
};

export default async function handler(req: NextRequest) {
  console.log("CRON request received");
  console.log("req:", req);
}
