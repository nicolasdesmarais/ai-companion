import {
  DataSourceItemList,
  KnowledgeOriginalContent,
} from "@/src/adapter-out/knowledge/types/DataSourceTypes";
import { Document } from "@langchain/core/documents";

export enum DomainEvent {
  DATASOURCE_INITIALIZED = "datasource.initialized",
  DATASOURCE_REFRESH_REQUESTED = "datasource.refresh.requested",
  DATASOURCE_ITEM_LIST_RECEIVED = "datasource.item.list.received",
  DATASOURCE_DELETE_REQUESTED = "datasource.delete.requested",
  DATASOURCE_MIGRATION_REQUESTED = "datasource.migration.requested",
  KNOWLEDGE_INITIALIZED = "knowledge.initialized",
  KNOWLEDGE_CONTENT_RETRIEVED = "knowledge.content.received",
  KNOWLEDGE_CHUNK_RECEIVED = "knowledge.chunk.received",
}

export interface DataSourceInitializedPayload {
  dataSourceId: string;
}

export interface DataSourceItemListReceivedPayload {
  dataSourceId: string;
  dataSourceItemList: DataSourceItemList;
  forRefresh: boolean;
  forceRefresh: boolean;
}

export interface DataSourceRefreshRequestedPayload {
  dataSourceId: string;
  forceRefresh: boolean;
}

export interface KnowledgeInitializedEventPayload {
  dataSourceId: string;
  knowledgeId: string;
}

export interface KnowledgeContentReceivedPayload {
  dataSourceId: string;
  knowledgeId: string;
  originalContent: KnowledgeOriginalContent;
}

export interface KnowledgeChunkReceivedPayload {
  chunk: Document[];
  chunkNumber: number;
}
