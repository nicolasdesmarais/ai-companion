import { BufferLoader } from "langchain/document_loaders/fs/buffer";
import { Document } from "langchain/document";
import { extractRawText } from "mammoth";

/**
 * Slightly modified from original source to remove dynamic import of mammoth (crashes in Next.js)
 * https://github.com/langchain-ai/langchainjs/blob/main/langchain/src/document_loaders/fs/docx.ts
 */
export class DocxLoader extends BufferLoader {
  constructor(filePathOrBlob: string | Blob) {
    super(filePathOrBlob);
  }

  public async parse(
    raw: Buffer,
    metadata: Document["metadata"]
  ): Promise<Document[]> {
    const docx = await extractRawText({
      buffer: raw,
    });

    if (!docx.value) return [];

    return [
      new Document({
        pageContent: docx.value,
        metadata,
      }),
    ];
  }
}
