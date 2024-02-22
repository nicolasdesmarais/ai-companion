import { Knowledge } from "@prisma/client";
import {
  DataSourceItem,
  DataSourceItemList,
  RetrieveContentAdapterResponse,
} from "./DataSourceTypes";
import { IndexKnowledgeResponse } from "./IndexKnowledgeResponse";

export interface DataSourceAdapter {
  getDataSourceItemList(
    orgId: string,
    userId: string,
    dataSourceId: string,
    data: any,
    forRefresh?: boolean,
    forceRefresh?: boolean
  ): Promise<DataSourceItemList>;

  shouldReindexKnowledge(knowledge: Knowledge, item: DataSourceItem): boolean;

  pollKnowledgeIndexingStatus(
    knowledge: Knowledge
  ): Promise<IndexKnowledgeResponse>;

  deleteKnowledge(knowledgeId: string): Promise<void>;

  getRemovedKnowledgeIds(
    dataSourceItemList: DataSourceItemList
  ): Promise<string[]>;
}

export interface ContentRetrievingDataSourceAdapter extends DataSourceAdapter {
  retrieveKnowledgeContent(
    orgId: string,
    userId: string,
    knowledge: Knowledge,
    data: any
  ): Promise<RetrieveContentAdapterResponse>;
}
