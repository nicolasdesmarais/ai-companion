import { redirect } from "next/navigation";
import { NextRequest, NextResponse } from "next/server";
import oauthTokenService from "@/src/domain/services/OAuthTokenService";
import { auth } from "@clerk/nextjs";
import { OAuthTokenProvider } from "@prisma/client";

export async function GET(req: NextRequest, res: NextResponse) {
  const { orgId, userId } = await auth();
  if (!orgId || !userId) {
    return NextResponse.json("Unauthorized", { status: 401 });
  }
  const { searchParams } = new URL(req.url);

  await oauthTokenService.handleOAuthCallback(
    orgId,
    userId,
    OAuthTokenProvider.MSFT,
    searchParams
  );

  const url = req.nextUrl.clone();
  url.pathname = "/close";
  return NextResponse.redirect(url);
}
