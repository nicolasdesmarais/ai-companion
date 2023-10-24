import { NextResponse } from "next/server";
import { currentUser } from "@clerk/nextjs";
import { AIService } from "@/domain/services/AIService";
import { OAuthTokenService } from "@/domain/services/OAuthTokenService";
import { OAuthTokenProvider } from "@prisma/client";

export async function GET(
  req: Request,
  { params }: { params: { aiId: string } }
) {
  const user = await currentUser();
  if (!user) {
    return new NextResponse("Unauthorized", { status: 401 });
  }

  try {
    const oauthTokenService = new OAuthTokenService();
    const oauthTokens = await oauthTokenService.getOAuthTokens(
      OAuthTokenProvider.GOOGLE,
      user.id
    );

    return NextResponse.json(oauthTokens);
  } catch (e) {
    return new NextResponse(e.message, { status: 500 });
  }
}
