import { Document } from "@langchain/core/documents";
import { OpenAIEmbeddings } from "@langchain/openai";
import { PineconeStore } from "@langchain/pinecone";
import { Pinecone } from "@pinecone-database/pinecone";
import { Redis } from "@upstash/redis";
import axios from "axios";

const embeddingsConfig = {
  azureOpenAIApiKey: process.env.AZURE_GPT40_KEY,
  azureOpenAIApiVersion: "2023-05-15",
  azureOpenAIApiInstanceName: "prod-appdirectai-east2",
  azureOpenAIApiDeploymentName: "text-embedding-ada-002",
  batchSize: 16,
  maxConcurrency: 1,
};

export type AIKey = {
  aiName: string;
  modelName: string;
  userId: string;
};

export class MemoryManager {
  private static instance: MemoryManager;
  private history: Redis;
  private pinecone: Pinecone;

  public constructor() {
    this.history = Redis.fromEnv();
    this.pinecone = new Pinecone();
  }

  public async vectorUpload(docs: Document[]) {
    const pineconeIndex = this.pinecone.Index(
      process.env.PINECONE_INDEX! || ""
    );

    const embeddings = new OpenAIEmbeddings(embeddingsConfig);

    const docIds = docs.map((doc, index) => {
      return `${doc.metadata.knowledge}#${index}`;
    });
    const pineconeStore = new PineconeStore(embeddings, { pineconeIndex });
    await pineconeStore.addDocuments(docs, { ids: docIds });
  }

  public async vectorSearch(
    query: string,
    knowledgeIds: string[],
    numDocs = 100
  ) {
    const pineconeIndex = this.pinecone.Index(
      process.env.PINECONE_INDEX! || ""
    );

    const vectorStore = await PineconeStore.fromExistingIndex(
      new OpenAIEmbeddings(embeddingsConfig),
      { pineconeIndex }
    );

    const similarDocs = await vectorStore
      .similaritySearch(query, numDocs, {
        knowledge: { $in: knowledgeIds },
      })
      .catch((err) => {
        console.log("WARNING: failed to get vector search results.", err);
      });
    return similarDocs;
  }

  public async vectorIdList(knowledgeId: string): Promise<string[]> {
    const host = process.env.PINECONE_INDEX_HOST;
    if (!host) {
      throw new Error("PINECONE_HOST is not set");
    }

    const response = await axios.get(
      `${host}/vectors/list?prefix=${knowledgeId}#`,
      {
        headers: {
          "Api-Key": process.env.PINECONE_API_KEY,
        },
      }
    );

    return response.data.vectors.map((v: any) => v.id as string);
  }

  public async vectorDelete(knowledgeId: string) {
    const vectorIds = await this.vectorIdList(knowledgeId);
    if (vectorIds.length === 0) {
      return;
    }

    const pineconeIndex = this.pinecone.Index(
      process.env.PINECONE_INDEX! || ""
    );

    await pineconeIndex.deleteMany(vectorIds);
  }

  public static getInstance(): MemoryManager {
    if (!MemoryManager.instance) {
      MemoryManager.instance = new MemoryManager();
    }
    return MemoryManager.instance;
  }
}
