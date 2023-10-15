// pages/api/searchFiles.js
import prismadb from "@/lib/prismadb";
import { currentUser } from "@clerk/nextjs";
import fs from "fs";
import { google } from "googleapis";
import { PDFLoader } from "langchain/document_loaders/fs/pdf";
import { RecursiveCharacterTextSplitter } from "langchain/text_splitter";
import { NextResponse } from "next/server";
import { Readable } from "stream";

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
    process.env.GOOGLE_CALLBACK_URL
  );

  const oauthToken = await prismadb.oAuthToken.findUnique({
    where: {
      provider_userId: {
        userId,
        provider: "GOOGLE",
      },
    },
  });

  if (!oauthToken?.data) {
    return;
  }

  const data = oauthToken.data as {
    access_token: string;
    refresh_token: string;
  };

  oauth2Client.setCredentials({
    access_token: data.access_token,
    refresh_token: data.refresh_token,
  });

  const drive = google.drive({ version: "v3", auth: oauth2Client });
  const query = `name contains '${searchTerm}'`;
  const googleRes = await drive.files.list({ q: query });
  const files = googleRes.data.files || [];
  const fileNames = files.map((file) => file.name);

  if (files.length > 0) {
    const file = files[0];
    const fileId = file.id;
    if (fileId) {
      const response = await drive.files.get(
        {
          fileId: fileId,
          alt: "media",
        },
        { responseType: "stream" }
      );

      const filePath = `/tmp/${file.name}`;
      const writableStream = fs.createWriteStream(filePath);

      if (response.data instanceof Readable) {
        response.data.pipe(writableStream).on("finish", async () => {
          console.log(`File downloaded to ${filePath}`);

          // Your async code goes here
          await loadPdf(filePath);
        });
      }
    }
  }

  return NextResponse.json(fileNames);
}

const loadPdf = async (filePath: string) => {
  const pdfLoader = new PDFLoader(filePath);
  const docs = await pdfLoader.load();
  console.log(docs);

  const splitter = new RecursiveCharacterTextSplitter({
    chunkSize: 4000,
    chunkOverlap: 600,
  });

  const docOutput = await splitter.splitDocuments(docs);
};
