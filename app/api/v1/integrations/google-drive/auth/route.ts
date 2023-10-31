import { google } from "googleapis";
import { redirect } from "next/navigation";
import { NextRequest, NextResponse } from "next/server";

const OAUTH2_CLIENT = new google.auth.OAuth2(
  process.env.GOOGLE_CLIENT_ID,
  process.env.GOOGLE_CLIENT_SECRET,
  process.env.GOOGLE_CALLBACK_URL
);

const SCOPE = [
  "https://www.googleapis.com/auth/drive.readonly",
  "https://www.googleapis.com/auth/userinfo.email",
  "https://www.googleapis.com/auth/userinfo.profile",
];

export async function GET(req: NextRequest, res: NextResponse) {
  const url = OAUTH2_CLIENT.generateAuthUrl({
    prompt: "consent",
    access_type: "offline",
    scope: SCOPE,
  });
  redirect(url);
}
