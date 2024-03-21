import { BadRequestError } from "@/src/domain/errors/Errors";
import { MemoryManager } from "@/src/lib/memory";
import { getTokenLength } from "@/src/lib/tokenCount";
import { Document } from "@langchain/core/documents";
import { fileTypeFromBlob } from "file-type";
import { writeFile } from "fs/promises";
import { CSVLoader } from "langchain/document_loaders/fs/csv";
import { EPubLoader } from "langchain/document_loaders/fs/epub";
import { JSONLoader } from "langchain/document_loaders/fs/json";
import { PDFLoader } from "langchain/document_loaders/fs/pdf";
import { TextLoader } from "langchain/document_loaders/fs/text";
import {
  MarkdownTextSplitter,
  RecursiveCharacterTextSplitter,
} from "langchain/text_splitter";

import mime from "mime-types";
import { DocxLoader } from "./DocxLoader";

export class FileLoader {
  private async getFilepath(file: Blob, filename: string) {
    if (!file) {
      throw new Error("Error reading file");
    }
    const bytes = await file.arrayBuffer();
    const buffer = Buffer.from(bytes);
    const path = `/tmp/${filename}`;
    await writeFile(path, buffer);
    return path;
  }
  public async getLangchainDocs(
    knowledgeId: string,
    filename: string,
    mimeType: string,
    filePathOrBlob: string | Blob
  ) {
    let docs;
    if (!mimeType) {
      mimeType = await this.computeMimeType(filename, filePathOrBlob);
    }
    console.log(`Loading file ${filename} with mime type ${mimeType}`);

    try {
      if (mimeType === "text/csv") {
        const loader = new CSVLoader(filePathOrBlob);
        docs = await loader.load();
      } else if (mimeType === "text/plain" || mimeType === "text/markdown") {
        const loader = new TextLoader(filePathOrBlob);
        docs = await loader.load();
      } else if (
        mimeType === "application/epub+zip" &&
        filePathOrBlob instanceof Blob
      ) {
        const path = await this.getFilepath(filePathOrBlob, filename);
        const loader = new EPubLoader(path);
        docs = await loader.load();
      } else if (mimeType === "application/epub+zip") {
        const loader = new EPubLoader(filePathOrBlob as string);
        docs = await loader.load();
      } else if (
        mimeType ===
        "application/vnd.openxmlformats-officedocument.wordprocessingml.document"
      ) {
        const loader = new DocxLoader(filePathOrBlob);
        docs = await loader.load();
      } else if (mimeType === "application/pdf") {
        const loader = new PDFLoader(filePathOrBlob);
        docs = await loader.load();
      } else if (mimeType === "application/json") {
        const loader = new JSONLoader(filePathOrBlob);
        docs = await loader.load();
      } else {
        throw new BadRequestError(`Unsupported file type ${mimeType}`);
      }

      let splitter;
      if (mimeType === "text/markdown") {
        splitter = new MarkdownTextSplitter();
      } else {
        splitter = new RecursiveCharacterTextSplitter({
          chunkSize: 4000,
          chunkOverlap: 600,
        });
      }

      const docOutput = await splitter.splitDocuments(docs);

      let totalTokenCount = 0;
      for (const doc of docOutput) {
        doc.metadata.source = filename;
        doc.metadata.knowledge = knowledgeId;
        const tokenCount = getTokenLength(doc.pageContent);
        totalTokenCount += tokenCount;
        doc.metadata.tokenCount = tokenCount;
        delete doc.metadata.pdf;
      }

      return {
        docs: docOutput,
        metadata: {
          documentCount: docOutput.length,
          totalTokenCount,
        },
      };
    } catch (e) {
      console.error("[FILE LOADER]", e, e.response?.data?.error);
      throw new Error(`Error loading file ${filename}`);
    }
  }

  public async loadDocs(docs: Document[], chunkNumber: number) {
    const memoryManager = await MemoryManager.getInstance();

    const docIds = docs.map((doc, index) => {
      return `${doc.metadata.knowledge}#${chunkNumber}#${index}`;
    });

    await memoryManager.vectorUpload(docs, docIds);
    return docIds;
  }

  public async loadJsonArray(jsonArray: any[], knowlegeId: string) {
    let totalTokenCount = 0;
    const docs: Document[] = jsonArray.map((json) => {
      const pageContent = JSON.stringify(json);
      const tokenCount = getTokenLength(pageContent);
      totalTokenCount += tokenCount;
      return new Document({
        pageContent,
        metadata: { knowledge: knowlegeId, tokenCount },
      });
    });

    const splitter = new RecursiveCharacterTextSplitter({
      chunkSize: 4000,
      chunkOverlap: 600,
    });

    const docOutput = await splitter.splitDocuments(docs);
    const docIds = docOutput.map((doc, index) => {
      return `${doc.metadata.knowledge}#${index}`;
    });

    const memoryManager = await MemoryManager.getInstance();
    await memoryManager.vectorUpload(docOutput, docIds);

    return {
      documentCount: docOutput.length,
      totalTokenCount,
    };
  }

  public async computeMimeType(filename: string, file: string | Blob) {
    let result;
    if (file instanceof Blob) {
      result = await fileTypeFromBlob(file);
    }
    if (result) {
      return result.mime;
    }
    return mime.lookup(filename) || "";
  }
}

const fileLoader = new FileLoader();
export default fileLoader;
