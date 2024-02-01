import { DataSourceItemList } from "@/src/adapter-out/knowledge/types/DataSourceItemList";

export enum DomainEvent {
  DATASOURCE_INITIALIZED = "datasource.initialized",
  DATASOURCE_REFRESH_REQUESTED = "datasource.refresh.requested",
  DATASOURCE_ITEM_LIST_RECEIVED = "datasource.item.list.received",
  DATASOURCE_DELETE_REQUESTED = "datasource.delete.requested",
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
