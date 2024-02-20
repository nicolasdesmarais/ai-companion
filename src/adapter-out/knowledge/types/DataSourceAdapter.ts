import { Knowledge } from "@prisma/client";
import {
  DataSourceItem,
  DataSourceItemList,
  RetrieveContentResponse,
} from "./DataSourceTypes";
import { IndexKnowledgeResponse } from "./IndexKnowledgeResponse";
import { KnowledgeIndexingResult } from "./KnowlegeIndexingResult";
import { OrgAndKnowledge } from "./OrgAndKnowledge";

export interface DataSourceAdapter {
  getDataSourceItemList(
    orgId: string,
    userId: string,
    dataSourceId: string,
    data: any,
    forRefresh?: boolean,
    forceRefresh?: boolean
  ): Promise<DataSourceItemList>;

  indexKnowledge(
    orgId: string,
    userId: string,
    knowledge: Knowledge,
    data: any
  ): Promise<IndexKnowledgeResponse>;

  shouldReindexKnowledge(knowledge: Knowledge, item: DataSourceItem): boolean;

  retrieveOrgAndKnowledgeIdFromEvent(data: any): OrgAndKnowledge;

  getKnowledgeResultFromEvent(
    knowledge: Knowledge,
    data: any
  ): Promise<KnowledgeIndexingResult>;

  loadKnowledgeResult(
    knowledge: Knowledge,
    result: KnowledgeIndexingResult,
    chunkCount: number
  ): Promise<IndexKnowledgeResponse>;

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
  ): Promise<RetrieveContentResponse>;
}
