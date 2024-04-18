import { Document, DocumentInterface } from "@langchain/core/documents";
import { OpenAIEmbeddings } from "@langchain/openai";
import { PineconeStore } from "@langchain/pinecone";
import { Index, Pinecone, RecordMetadata } from "@pinecone-database/pinecone";
import axios from "axios";

const embeddingsConfig = {
  azureOpenAIApiKey: process.env.AZURE_GPT35_KEY,
  azureOpenAIApiVersion: "2023-05-15",
  azureOpenAIApiInstanceName: "appdirect-prod-ai-useast",
  azureOpenAIApiDeploymentName: "ai-prod-ada2",
  batchSize: 16,
  maxConcurrency: 3,
};

const pineconeIndexName = process.env.PINECONE_INDEX!;
const pineconeHost = process.env.PINECONE_INDEX_HOST!;

export type AIKey = {
  aiName: string;
  modelName: string;
  userId: string;
};

export class MemoryManager {
  private static instance: MemoryManager;
  private pinecone: Pinecone;
  private pineconeIndex: Index<RecordMetadata>;

  public constructor() {
    this.pinecone = new Pinecone();
    this.pineconeIndex = this.pinecone.Index(pineconeIndexName, pineconeHost);
  }

  public async vectorUpload(docs: Document[], docIds: string[]) {
    const embeddings = new OpenAIEmbeddings(embeddingsConfig);

    const pineconeStore = new PineconeStore(embeddings, {
      pineconeIndex: this.pineconeIndex,
    });
    await pineconeStore.addDocuments(docs, { ids: docIds });
  }

  public async vectorSearch(
    query: string,
    knowledgeIds: string[],
    numDocs = 100
  ): Promise<[DocumentInterface<Record<string, any>>, number][]> {
    const vectorStore = await PineconeStore.fromExistingIndex(
      new OpenAIEmbeddings(embeddingsConfig),
      { pineconeIndex: this.pineconeIndex }
    );

    let similarDocs: [DocumentInterface<Record<string, any>>, number][] = [];
    try {
      similarDocs = await vectorStore.similaritySearchWithScore(
        query,
        numDocs,
        {
          knowledge: { $in: knowledgeIds },
        }
      );
    } catch (err) {
      console.log("WARNING: failed to get vector search results.", err);
    }

    return similarDocs;
  }

  public async vectorIdList(
    knowledgeId: string,
    paginationToken?: string
  ): Promise<{ vectorIds: string[]; paginationNextToken: string | undefined }> {
    let requestUrl = `${pineconeHost}/vectors/list?prefix=${knowledgeId}#`;
    if (paginationToken) {
      requestUrl += `&paginationToken=${paginationToken}`;
    }

    const response = await axios.get(requestUrl, {
      headers: {
        "Api-Key": process.env.PINECONE_API_KEY,
      },
    });
    const vectorIds: string[] = response.data.vectors.map(
      (v: any) => v.id as string
    );
    const paginationNextToken = response.data.pagination?.next;

    return { vectorIds, paginationNextToken };
  }

  public async vectorDelete(vectorIds: string[]) {
    if (vectorIds.length === 0) {
      return;
    }

    await this.pineconeIndex.deleteMany(vectorIds);
  }

  public static getInstance(): MemoryManager {
    if (!MemoryManager.instance) {
      MemoryManager.instance = new MemoryManager();
    }
    return MemoryManager.instance;
  }
}
