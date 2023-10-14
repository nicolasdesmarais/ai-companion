import { OAuthTokenService } from "@/domain/services/OAuthTokenService";
import { auth } from "@clerk/nextjs";
import { OAuthTokenProvider } from "@prisma/client";
import { google } from "googleapis";
import { NextRequest, NextResponse } from "next/server";

const oauth2Client = new google.auth.OAuth2(
  process.env.GOOGLE_CLIENT_ID,
  process.env.GOOGLE_CLIENT_SECRET,
  process.env.GOOGLE_CALLBACK_URL
);

export async function GET(req: NextRequest): Promise<NextResponse> {
  const authentication = await auth();
  const userId = authentication?.userId;
  if (!userId) {
    return NextResponse.json("Unauthorized", { status: 401 });
  }

  const { searchParams } = new URL(req.url);
  const code = searchParams.get("code");
  try {
    const { tokens } = await oauth2Client.getToken(code as string);

    const provider = OAuthTokenProvider.GOOGLE;
    const oauthTokenService = new OAuthTokenService();
    oauthTokenService.upsertToken({
      userId,
      provider,
      data: JSON.stringify(tokens),
    });

    // You can now use the Google Drive API. Store the tokens securely (in session, JWT, database, etc.)
    return NextResponse.json("Successfully authenticated!");
  } catch (error) {
    console.log(error);
    return NextResponse.json("Authentication failed", { status: 400 });
  }
}
