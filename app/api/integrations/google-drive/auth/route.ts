import { google } from "googleapis";
import { NextApiRequest, NextApiResponse } from "next";
import { redirect } from "next/navigation";

const oauth2Client = new google.auth.OAuth2(
  process.env.GOOGLE_CLIENT_ID,
  process.env.GOOGLE_CLIENT_SECRET,
  "http://localhost:3000/api/integrations/google-drive/callback" // This should match the authorized redirect URI set in Google Developers Console.
);

export async function GET(req: NextApiRequest, res: NextApiResponse) {
  const url = oauth2Client.generateAuthUrl({
    access_type: "offline",
    scope: ["https://www.googleapis.com/auth/drive.readonly"], // Adjust scopes based on your requirements.
  });
  redirect(url);
}
