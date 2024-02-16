import { DataSourceItemList } from "@/src/adapter-out/knowledge/types/DataSourceItemList";
import { KnowledgeIndexingResult } from "@/src/adapter-out/knowledge/types/KnowlegeIndexingResult";
import { DataSourceType } from "@prisma/client";

export enum DomainEvent {
  DATASOURCE_INITIALIZED = "datasource.initialized",
  DATASOURCE_REFRESH_REQUESTED = "datasource.refresh.requested",
  DATASOURCE_ITEM_LIST_RECEIVED = "datasource.item.list.received",
  DATASOURCE_DELETE_REQUESTED = "datasource.delete.requested",
  DATASOURCE_MIGRATION_REQUESTED = "datasource.migration.requested",
  KNOWLEDGE_INITIALIZED = "knowledge.initialized",
  KNOWLEDGE_EVENT_RECEIVED = "knowledge.event.received",
  KNOWLEDGE_CHUNK_RECEIVED = "knowledge.chunk.received",
}

export interface DataSourceItemListReceivedPayload {
  dataSourceId: string;
  dataSourceItemList: DataSourceItemList;
  forRefresh: boolean;
}

export interface KnowledgeInitializedEventPayload {
  dataSourceId: string;
  knowledgeId: string;
}

export interface KnowledgeChunkReceivedPayload {
  orgId: string;
  dataSourceType: DataSourceType;
  knowledgeIndexingResult: KnowledgeIndexingResult;
  index: number;
}
