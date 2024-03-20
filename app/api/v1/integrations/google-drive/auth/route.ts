import oauthTokenService from "@/src/domain/services/OAuthTokenService";
import { auth } from "@clerk/nextjs";
import { OAuthTokenProvider } from "@prisma/client";
import { redirect } from "next/navigation";
import { NextRequest, NextResponse } from "next/server";

export const dynamic = "force-dynamic";

export async function GET(req: NextRequest, res: NextResponse) {
  const { orgId } = auth();
  if (!orgId) {
    return new Response("Unauthorized", { status: 401 });
  }

  const redirectUrl = await oauthTokenService.getOAuthRedirectUrl(
    orgId,
    OAuthTokenProvider.GOOGLE
  );

  redirect(redirectUrl);
}
