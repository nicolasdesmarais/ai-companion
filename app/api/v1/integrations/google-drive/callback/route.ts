import oauthTokenService from "@/src/domain/services/OAuthTokenService";
import { withErrorHandler } from "@/src/middleware/ErrorMiddleware";
import { auth } from "@clerk/nextjs";
import { OAuthTokenProvider } from "@prisma/client";
import { NextRequest, NextResponse } from "next/server";

async function getHandler(req: NextRequest): Promise<NextResponse> {
  const { orgId, userId } = await auth();
  if (!orgId || !userId) {
    return NextResponse.json("Unauthorized", { status: 401 });
  }

  const { searchParams } = new URL(req.url);

  await oauthTokenService.handleOAuthCallback(
    orgId,
    userId,
    OAuthTokenProvider.GOOGLE,
    searchParams
  );

  const url = req.nextUrl.clone();
  url.pathname = "/close";
  return NextResponse.redirect(url);
}

export const GET = withErrorHandler(getHandler);
