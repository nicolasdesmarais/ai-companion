// pages/api/searchFiles.js
import { GoogleDriveLoader } from "@/domain/services/knowledge/GoogleDriveLoader";
import { currentUser } from "@clerk/nextjs";
import { PDFLoader } from "langchain/document_loaders/fs/pdf";
import { RecursiveCharacterTextSplitter } from "langchain/text_splitter";
import { NextResponse } from "next/server";

export async function POST(req: Request) {
  const user = await currentUser();
  if (!user) {
    return new NextResponse("Unauthorized", { status: 401 });
  }

  const body = await req.json();

  const userId = user.id;
  const { searchTerm } = body;

  const googleDriveLoader = new GoogleDriveLoader();
  const filenames = await googleDriveLoader.loadFolder(userId, searchTerm);

  return NextResponse.json(filenames);

  // const drive = google.drive({ version: "v3", auth: oauth2Client });
  // const query = `name contains '${searchTerm}'`;
  // const googleRes = await drive.files.list({ q: query });
  // const files = googleRes.data.files || [];
  // const fileNames = files.map((file) => file.name);

  // if (files.length > 0) {
  //   const file = files[0];
  //   const fileId = file.id;
  //   if (fileId) {
  //     const response = await drive.files.get(
  //       {
  //         fileId: fileId,
  //         alt: "media",
  //       },
  //       { responseType: "stream" }
  //     );

  //     const filePath = `/tmp/${file.name}`;
  //     const writableStream = fs.createWriteStream(filePath);

  //     if (response.data instanceof Readable) {
  //       response.data.pipe(writableStream).on("finish", async () => {
  //         console.log(`File downloaded to ${filePath}`);

  //         // Your async code goes here
  //         await loadPdf(filePath);
  //       });
  //     }
  //   }
  // }

  // return NextResponse.json(fileNames);
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
