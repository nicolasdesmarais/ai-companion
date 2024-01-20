import oauthTokenService from "@/src/domain/services/OAuthTokenService";
import { withErrorHandler } from "@/src/middleware/ErrorMiddleware";
import { auth } from "@clerk/nextjs";
import { OAuthTokenProvider } from "@prisma/client";
import { NextResponse } from "next/server";

async function getHandler(req: Request) {
  const { orgId, userId } = await auth();
  if (!orgId || !userId) {
    return new NextResponse("Unauthorized", { status: 401 });
  }

  const oauthTokens = await oauthTokenService.getOAuthTokens(
    orgId,
    userId,
    OAuthTokenProvider.MSFT
  );

  return NextResponse.json(oauthTokens);
}

export const GET = withErrorHandler(getHandler);
