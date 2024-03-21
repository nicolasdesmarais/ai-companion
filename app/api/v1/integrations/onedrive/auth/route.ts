import msftOAuthAdapter from "@/src/adapter-out/oauth/MsftOAuthAdapter";
import { redirect } from "next/navigation";
import { NextRequest, NextResponse } from "next/server";

export const dynamic = "force-dynamic";

export async function GET(req: NextRequest, res: NextResponse) {
  const redirectUrl = await msftOAuthAdapter.getOAuthRedirectUrl();
  redirect(redirectUrl);
}
