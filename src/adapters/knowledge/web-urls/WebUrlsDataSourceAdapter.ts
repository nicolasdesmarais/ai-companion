import { BadRequestError } from "@/src/domain/errors/Errors";
import { ApifyWebhookEvent } from "@/src/domain/types/ApifyWebhookEvent";
import { logWithTimestamp } from "@/src/lib/logging";
import {
  DataSourceType,
  Knowledge,
  KnowledgeIndexStatus,
} from "@prisma/client";
import { put } from "@vercel/blob";
import axios from "axios";
import fileLoader from "../knowledgeLoaders/FileLoader";
import { DataSourceAdapter } from "../types/DataSourceAdapter";
import { DataSourceItemList } from "../types/DataSourceItemList";
import { IndexKnowledgeResponse } from "../types/IndexKnowledgeResponse";
import {
  KnowledgeIndexingResult,
  KnowledgeIndexingResultStatus,
} from "../types/KnowlegeIndexingResult";
import apifyAdapter from "./ApifyAdapter";
import { WebUrlDataSourceInput } from "./types/WebUrlDataSourceInput";
import { WebUrlMetadata } from "./types/WebUrlMetadata";

export class WebUrlsDataSourceAdapter implements DataSourceAdapter {
  public async getDataSourceItemList(
    orgId: string,
    userId: string,
    data: any
  ): Promise<DataSourceItemList> {
    const input = data as WebUrlDataSourceInput;
    const result: DataSourceItemList = {
      dataSourceName: input.url,
      items: [
        {
          name: input.url,
          type: DataSourceType.WEB_URL,
        },
      ],
    };
    return result;
  }

  public async indexKnowledge(
    orgId: string,
    userId: string,
    knowledge: Knowledge,
    data: any
  ): Promise<IndexKnowledgeResponse> {
    const input = data as WebUrlDataSourceInput;
    console.log(`Indexing web url ${input.url}`);
    const actorRunId = await apifyAdapter.startUrlIndexing(
      knowledge.id,
      input.url
    );

    if (!actorRunId) {
      throw new Error("Failed to start web indexing run");
    }

    console.log(`Started web indexing run ${actorRunId}`);

    const metadata: WebUrlMetadata = {
      indexingRunId: actorRunId,
    };

    return {
      indexStatus: KnowledgeIndexStatus.INDEXING,
      metadata,
    };
  }

  public retrieveKnowledgeIdFromEvent(data: any): string {
    const event = data as ApifyWebhookEvent;
    return event.knowledgeId;
  }

  public async getKnowledgeResultFromEvent(
    knowledge: Knowledge,
    data: any
  ): Promise<KnowledgeIndexingResult> {
    const event = data as ApifyWebhookEvent;
    const metadata = knowledge.metadata as unknown as WebUrlMetadata;
    const actorRunId = event.eventData.actorRunId;
    if (actorRunId !== metadata.indexingRunId) {
      throw new BadRequestError("Event actorRunId does not match metadata");
    }

    return await this.getActorRunResult(knowledge, metadata);
  }

  public async loadKnowledgeResult(
    knowledge: Knowledge,
    result: KnowledgeIndexingResult
  ): Promise<IndexKnowledgeResponse> {
    if (!result.blobUrl) {
      return {
        indexStatus: KnowledgeIndexStatus.FAILED,
      };
    }

    logWithTimestamp(`Loading blob content for knowledge ${knowledge.id}`);
    const blob = await this.getBlobContent(result.blobUrl);
    logWithTimestamp(`Loaded blob content for knowledge ${knowledge.id}`);

    const { documentCount, totalTokenCount } = await fileLoader.loadFile(
      knowledge.id,
      knowledge.name,
      "text/csv",
      blob
    );

    logWithTimestamp(`Loaded file for knowledge ${knowledge.id}`);

    let indexStatus;
    switch (result.status) {
      case KnowledgeIndexingResultStatus.SUCCESSFUL:
        indexStatus = KnowledgeIndexStatus.COMPLETED;
        break;
      case KnowledgeIndexingResultStatus.PARTIAL:
        indexStatus = KnowledgeIndexStatus.PARTIALLY_COMPLETED;
        break;
      case KnowledgeIndexingResultStatus.FAILED:
        indexStatus = KnowledgeIndexStatus.FAILED;
    }

    return {
      indexStatus,
      blobUrl: result.blobUrl,
      metadata: {
        documentCount,
        totalTokenCount,
      },
    };
  }

  public async pollKnowledgeIndexingStatus(
    knowledge: Knowledge
  ): Promise<IndexKnowledgeResponse> {
    const metadata = knowledge.metadata as unknown as WebUrlMetadata;
    if (!metadata?.indexingRunId) {
      return {
        indexStatus: KnowledgeIndexStatus.FAILED,
      };
    }

    // return this.getActorRunResult(knowledge, metadata);
    //TODO: Re-implement
    return {
      indexStatus: KnowledgeIndexStatus.INDEXING,
    };
  }

  public async deleteKnowledge(knowledgeId: string): Promise<void> {
    fileLoader.deleteKnowledge(knowledgeId);
  }

  private async getActorRunResult(
    knowledge: Knowledge,
    metadata: WebUrlMetadata
  ) {
    logWithTimestamp(
      `Retrieving actor run result for indexingRunId=${metadata.indexingRunId}`
    );
    const result = await apifyAdapter.getActorRunResult(metadata.indexingRunId);
    logWithTimestamp(
      `Retrieved actor run result for indexingRunId=${metadata.indexingRunId}`
    );

    let blobUrl;
    if (
      result.items &&
      (result.status === KnowledgeIndexingResultStatus.PARTIAL ||
        result.status === KnowledgeIndexingResultStatus.SUCCESSFUL)
    ) {
      const itemsBlob = this.bufferToBlob(result.items);
      const cloudBlob = await put(`${knowledge.name}.json`, itemsBlob, {
        access: "public",
      });
      blobUrl = cloudBlob.url;
    }

    return {
      status: result.status,
      blobUrl,
    };

    // const { documentCount, totalTokenCount } = await fileLoader.loadFile(
    //   knowledge.id,
    //   knowledge.name,
    //   "text/csv",
    //   itemsBlob
    // );

    // return {
    //   indexStatus: KnowledgeIndexStatus.INDEXING,
    //   blobUrl: cloudBlob.url,
    // };
  }

  private bufferToBlob(buffer: Buffer): Blob {
    const arrayBuffer = new ArrayBuffer(buffer.length);
    const view = new Uint8Array(arrayBuffer);
    for (let i = 0; i < buffer.length; ++i) {
      view[i] = buffer[i];
    }

    const blob = new Blob([arrayBuffer]);

    return blob;
  }

  private async getBlobContent(url: string): Promise<Blob> {
    try {
      const response = await axios.get(url, {
        responseType: "blob",
      });
      return response.data;
    } catch (error) {
      if (axios.isAxiosError(error)) {
        console.error("Error fetching data: ", error.message);
        throw error;
      } else {
        console.error("Unexpected error: ", error);
        throw new Error("An unexpected error occurred");
      }
    }
  }
}

const webUrlsDataSourceAdapter = new WebUrlsDataSourceAdapter();
export default webUrlsDataSourceAdapter;
