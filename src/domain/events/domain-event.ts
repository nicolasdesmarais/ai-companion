import { DataSourceItemList } from "@/src/adapter-out/knowledge/types/DataSourceItemList";

export enum DomainEvent {
  DATASOURCE_INITIALIZED = "datasource.initialized",
  DATASOURCE_ITEM_LIST_RECEIVED = "datasource.item.list.received",
  KNOWLEDGE_INITIALIZED = "knowledge.initialized",
  KNOWLEDGE_EVENT_RECEIVED = "knowledge.event.received",
  KNOWLEDGE_CHUNK_RECEIVED = "knowledge.chunk.received",
}

export interface DataSourceItemListReceivedPayload {
  dataSourceId: string;
  dataSourceItemList: DataSourceItemList;
}
