import { NextResponse, NextRequest } from 'next/server';
import { writeFile } from 'fs/promises';
import { CSVLoader } from "langchain/document_loaders/fs/csv";
import { TextLoader } from "langchain/document_loaders/fs/text";
import { currentUser } from "@clerk/nextjs";
import prismadb from "@/lib/prismadb";
import { MemoryManager } from "@/lib/memory";
import { EPubLoader } from "langchain/document_loaders/fs/epub";
import { DocxLoader } from "langchain/document_loaders/fs/docx";
import { PDFLoader } from "langchain/document_loaders/fs/pdf";
import { RecursiveCharacterTextSplitter } from "langchain/text_splitter";

const getBlob = async (source: ReadableStream<Uint8Array>) => {
  const reader = source.getReader();
  const stream = new ReadableStream({
    start(controller) {
      return pump();
      function pump(): any {
        return reader.read().then(({ done, value }) => {
          if (done) {
            controller.close();
            return;
          }
          controller.enqueue(value);
          return pump();
        });
      }
    },
  });
  return (new Response(stream)).blob();
}

const getFilepath = async (request: NextRequest) => {
  const data = await request.formData();
  const file: File | null = data.get('file') as unknown as File

  if (!file) {
    throw new Error('Error reading file');
  }
  const bytes = await file.arrayBuffer();
  const buffer = Buffer.from(bytes);
  const path = `/tmp/${file.name}`;
  await writeFile(path, buffer);
  return path;
}

export async function POST(request: NextRequest): Promise<NextResponse> {
  const user = await currentUser();
  if (!user || !user.id) {
    return new NextResponse("Unauthorized", { status: 401 });
  }

  const { searchParams } = new URL(request.url);
  const filename = searchParams.get('filename');
  const type = searchParams.get('type');

  if (!filename || !type) {
    return new NextResponse("Missing required fields", { status: 400 });
  };
  try {
    if (request.body) {
      let docs;
      const data = await request.formData();
      const file: File | null = data.get('file') as unknown as File
      if (type === 'text/csv') {
        const loader = new CSVLoader(file, "text");
        docs = await loader.load(); 
      } else if (type === 'text/plain') {
        const blob = await getBlob(request.body);
        const loader = new TextLoader(blob);
        docs = await loader.load(); 
      } else if (type === 'application/epub+zip') {
        const path = await getFilepath(request);
        const loader = new EPubLoader(path);
        docs = await loader.load();
      } else if (type === 'application/vnd.openxmlformats-officedocument.wordprocessingml.document') {
        const blob = await getBlob(request.body);
        const loader = new DocxLoader(blob);
        docs = await loader.load();
      } else if (type === 'application/pdf') {
        const blob = await getBlob(request.body);
        const loader = new PDFLoader(blob);
        docs = await loader.load();
      } else {
        return NextResponse.json("Unsupported file format.", { status: 400 });
      }
  
      const knowledge = await prismadb.knowledge.create({
        data: {
          userId: user.id,
          name: filename,
          type,
        }
      });
  
      for (const doc of docs) {
        doc.metadata.source = filename
        doc.metadata.knowledge = knowledge.id
      }

      const splitter = new RecursiveCharacterTextSplitter({
        chunkSize: 4000,
        chunkOverlap: 600,
      });
      
      const docOutput = await splitter.splitDocuments(docs);
console.log(docOutput)
      const memoryManager = await MemoryManager.getInstance();
      await memoryManager.vectorUpload(docOutput);
      return NextResponse.json(knowledge);
    } else {
      return NextResponse.json("Missing file", { status: 400 });
    }
  } catch (error) {
    if (error.response?.data?.error?.message) {
      return new NextResponse(error.response.data.error.message, { status: 422 });
    }
    console.log('[KNOWLEDGE_ERROR]', error);
    return new NextResponse("Internal Error", { status: 500 });
  }
}
