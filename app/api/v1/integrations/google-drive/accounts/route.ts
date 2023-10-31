import oauthTokenService from "@/src/domain/services/OAuthTokenService";
import { currentUser } from "@clerk/nextjs";
import { OAuthTokenProvider } from "@prisma/client";
import { NextResponse } from "next/server";

export async function GET(
  req: Request,
  { params }: { params: { aiId: string } }
) {
  const user = await currentUser();
  if (!user) {
    return new NextResponse("Unauthorized", { status: 401 });
  }

  try {
    const oauthTokens = await oauthTokenService.getOAuthTokens(
      OAuthTokenProvider.GOOGLE,
      user.id
    );

    return NextResponse.json(oauthTokens);
  } catch (e) {
    return new NextResponse(e.message, { status: 500 });
  }
}
