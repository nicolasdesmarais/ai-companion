import oauthTokenService from "@/src/domain/services/OAuthTokenService";
import { withErrorHandler } from "@/src/middleware/ErrorMiddleware";
import { currentUser } from "@clerk/nextjs";
import { OAuthTokenProvider } from "@prisma/client";
import { NextResponse } from "next/server";

async function getHandler(req: Request) {
  const user = await currentUser();
  if (!user) {
    return new NextResponse("Unauthorized", { status: 401 });
  }

  const oauthTokens = await oauthTokenService.getOAuthTokens(
    OAuthTokenProvider.GOOGLE,
    user.id
  );

  return NextResponse.json(oauthTokens);
}

export const GET = withErrorHandler(getHandler);
