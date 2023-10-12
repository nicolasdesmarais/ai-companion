// pages/api/searchFiles.js
import prismadb from "@/lib/prismadb";
import { currentUser } from "@clerk/nextjs";
import { google } from "googleapis";
import { NextResponse } from "next/server";

export async function POST(req: Request) {
  const user = await currentUser();
  if (!user) {
    return new NextResponse("Unauthorized", { status: 401 });
  }

  const body = await req.json();

  const userId = user.id;
  const { searchTerm } = body;

  const oauth2Client = new google.auth.OAuth2(
    process.env.GOOGLE_CLIENT_ID,
    process.env.GOOGLE_CLIENT_SECRET,
    "http://localhost:3000/api/integrations/google-drive/callback"
  );

  const tokens = await prismadb.googleTokens.findUnique({
    where: {
      userId,
    },
  });

  oauth2Client.setCredentials({
    access_token: tokens?.accessToken,
    refresh_token: tokens?.refreshToken,
  });

  const drive = google.drive({ version: "v3", auth: oauth2Client });
  const query = `name contains '${searchTerm}'`;
  const googleRes = await drive.files.list({ q: query });
  const files = googleRes.data.files || [];
  const fileNames = files.map((file) => file.name);

  // if (files.length > 0) {
  //   const fileId = files[0].id;
  //   if (fileId) {
  //     const file = await drive.files.get({
  //       fileId: fileId,
  //       alt: "media",
  //     });

  //     // const fs = require("fs");
  //     // const dest = fs.createWriteStream("/tmp/google-drive.ext");
  //     // const pdfLoader = new PDFLoader(dest);
  //     // const docs = await pdfLoader.load();

  //     // const loader = new PDFLoader(fileRes.data);
  //   }
  // }

  return NextResponse.json(fileNames);
}
