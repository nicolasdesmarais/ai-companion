import { Document } from "langchain/document";

import { BadRequestError } from "@/domain/errors/Errors";
import { MemoryManager } from "@/lib/memory";
import prismadb from "@/lib/prismadb";
import { writeFile } from "fs/promises";
import { CSVLoader } from "langchain/document_loaders/fs/csv";
import { DocxLoader } from "langchain/document_loaders/fs/docx";
import { EPubLoader } from "langchain/document_loaders/fs/epub";
import { PDFLoader } from "langchain/document_loaders/fs/pdf";
import { TextLoader } from "langchain/document_loaders/fs/text";
import { RecursiveCharacterTextSplitter } from "langchain/text_splitter";
export class FileLoader {
  private async getFilepath(file: File) {
    if (!file) {
      throw new Error("Error reading file");
    }
    const bytes = await file.arrayBuffer();
    const buffer = Buffer.from(bytes);
    const path = `/tmp/${file.name}`;
    await writeFile(path, buffer);
    return path;
  }

  public async loadFileFromPath(
    userId: string,
    type: string,
    filename: string,
    filePath: string,
    blobUrl: string
  ) {
    let docs;

    if (type === "text/csv") {
      const loader = new CSVLoader(filePath, "text");
      docs = await loader.load();
    } else if (type === "text/plain") {
      const loader = new TextLoader(filePath);
      docs = await loader.load();
    } else if (type === "application/epub+zip") {
      const loader = new EPubLoader(filePath);
      docs = await loader.load();
    } else if (
      type ===
      "application/vnd.openxmlformats-officedocument.wordprocessingml.document"
    ) {
      const loader = new DocxLoader(filePath);
      docs = await loader.load();
    } else if (type === "application/pdf") {
      const loader = new PDFLoader(filePath);
      docs = await loader.load();
    } else {
      throw new BadRequestError("Unsupported file type");
    }

    const knowledge = await prismadb.knowledge.create({
      data: {
        userId: userId,
        name: filename,
        type,
        blobUrl,
      },
    });

    for (const doc of docs) {
      doc.metadata.source = filename;
      doc.metadata.knowledge = knowledge.id;
    }

    const splitter = new RecursiveCharacterTextSplitter({
      chunkSize: 4000,
      chunkOverlap: 600,
    });

    const docOutput = await splitter.splitDocuments(docs);

    const memoryManager = await MemoryManager.getInstance();
    await memoryManager.vectorUpload(docOutput);
    return knowledge;
  }

  public async loadJsonArray(jsonArray: any[], knowlegeId: string) {
    const docs: Document[] = jsonArray.map((json) => {
      return new Document({
        pageContent: JSON.stringify(json),
        metadata: { knowledge: knowlegeId },
      });
    });

    const splitter = new RecursiveCharacterTextSplitter({
      chunkSize: 4000,
      chunkOverlap: 600,
    });

    const docOutput = await splitter.splitDocuments(docs);

    const memoryManager = await MemoryManager.getInstance();
    await memoryManager.vectorUpload(docOutput);
  }
}
