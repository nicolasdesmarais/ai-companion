import { OAuthTokenService } from "@/src/domain/services/OAuthTokenService";
import { auth } from "@clerk/nextjs";
import { OAuthTokenProvider } from "@prisma/client";
import { Credentials } from "google-auth-library";
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
    const email = await getEmailAddressFromToken(tokens);
    if (!email) {
      throw new Error("Unable to get email address from token");
    }

    const provider = OAuthTokenProvider.GOOGLE;
    const oauthTokenService = new OAuthTokenService();
    await oauthTokenService.upsertToken({
      userId,
      provider,
      email: email,
      data: tokens,
    });

    const url = req.nextUrl.clone();
    url.pathname = "/close";
    return NextResponse.redirect(url);
  } catch (error) {
    console.error(error);
    return NextResponse.json("Authentication failed", { status: 400 });
  }
}

async function getEmailAddressFromToken(tokens: Credentials) {
  oauth2Client.setCredentials(tokens);
  const people = google.people({ version: "v1", auth: oauth2Client });

  const user = await people.people.get({
    resourceName: "people/me",
    personFields: "emailAddresses",
  });

  for (const emailAddress of user.data.emailAddresses || []) {
    if (emailAddress.metadata?.primary) {
      return emailAddress.value;
    }
  }
}
