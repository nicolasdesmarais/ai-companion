import { redirect } from "next/navigation";
import { NextRequest, NextResponse } from "next/server";
import msftOAuthAdapter from "@/src/adapter-out/oauth/MsftOAuthAdapter";

export async function GET(req: NextRequest, res: NextResponse) {
  const redirectUrl = await msftOAuthAdapter.getOAuthRedirectUrl();
  redirect(redirectUrl);
}
