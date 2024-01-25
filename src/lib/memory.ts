import { OpenAIEmbeddings } from "@langchain/openai";
import { PineconeStore } from "@langchain/pinecone";
import { Pinecone } from "@pinecone-database/pinecone";
import { Redis } from "@upstash/redis";
import { Document } from "langchain/document";

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

    await PineconeStore.fromDocuments(
      docs,
      new OpenAIEmbeddings(embeddingsConfig),
      {
        pineconeIndex,
      }
    );
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

  public async vectorDelete(knowledgeId: string) {
    const pineconeIndex = this.pinecone.Index(
      process.env.PINECONE_INDEX! || ""
    );

    await pineconeIndex.deleteMany({ knowledge: knowledgeId });
  }

  public static getInstance(): MemoryManager {
    if (!MemoryManager.instance) {
      MemoryManager.instance = new MemoryManager();
    }
    return MemoryManager.instance;
  }
}
