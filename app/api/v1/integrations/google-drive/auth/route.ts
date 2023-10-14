import { google } from "googleapis";
import { NextApiRequest, NextApiResponse } from "next";
import { redirect } from "next/navigation";

const OAUTH2_CLIENT = new google.auth.OAuth2(
  process.env.GOOGLE_CLIENT_ID,
  process.env.GOOGLE_CLIENT_SECRET,
  process.env.GOOGLE_CALLBACK_URL
);

const SCOPE = ["https://www.googleapis.com/auth/drive.readonly"];

export async function GET(req: NextApiRequest, res: NextApiResponse) {
  const url = OAUTH2_CLIENT.generateAuthUrl({
    access_type: "offline",
    scope: SCOPE,
  });
  redirect(url);
}
