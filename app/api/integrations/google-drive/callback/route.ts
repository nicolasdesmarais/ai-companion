import { google } from "googleapis";
import { NextRequest, NextResponse } from "next/server";

const oauth2Client = new google.auth.OAuth2(
  process.env.GOOGLE_CLIENT_ID,
  process.env.GOOGLE_CLIENT_SECRET,
  "http://localhost:3000/api/integrations/google-drive/callback"
);

export async function GET(req: NextRequest): Promise<NextResponse> {
  const { searchParams } = new URL(req.url);
  const code = searchParams.get("code");
  try {
    const { tokens } = await oauth2Client.getToken(code as string);
    oauth2Client.setCredentials(tokens);

    // You can now use the Google Drive API. Store the tokens securely (in session, JWT, database, etc.)
    return NextResponse.json("Successfully authenticated!");
  } catch (error) {
    return NextResponse.json("Authentication failed", { status: 400 });
  }
}
