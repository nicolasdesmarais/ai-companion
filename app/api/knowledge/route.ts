import { NextResponse } from 'next/server';
import { CSVLoader } from "langchain/document_loaders/fs/csv";
import { TextLoader } from "langchain/document_loaders/fs/text";
import { currentUser } from "@clerk/nextjs";
import prismadb from "@/lib/prismadb";
import { MemoryManager } from "@/lib/memory";

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

export async function POST(request: Request): Promise<NextResponse> {
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
  
  if (request.body) {
    const blob = await getBlob(request.body);
    let docs;
    if (type === 'text/csv') {
      const loader = new CSVLoader(blob, "text");
      docs = await loader.load(); 
    } else if (type === 'text/plain') {
      const loader = new TextLoader(blob);
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
    const memoryManager = await MemoryManager.getInstance();
    await memoryManager.vectorUpload(docs);
    return NextResponse.json(knowledge);
  } else {
    return NextResponse.json("Missing file", { status: 400 });
  }
}

export async function DELETE(
  request: Request,
  { params }: { params: { id: string } }
) {
  try {
    const user = await currentUser();
    if (!user || !user.id) {
      return new NextResponse("Unauthorized", { status: 401 });
    }

    const memoryManager = await MemoryManager.getInstance();
    await memoryManager.vectorDelete(params.id);

    await prismadb.knowledge.update({
      where: {
        id: params.id
      },
      data: {
        companions: {
          set: []
        }
      },
      include: {
        companions: true,
      }
    });
    
    const companion = await prismadb.knowledge.delete({
      where: {
        id: params.id
      }
    });

    return NextResponse.json(companion);
  } catch (error) {
    console.log("[COMPANION_DELETE]", error);
    return new NextResponse("Internal Error", { status: 500 });
  }
};