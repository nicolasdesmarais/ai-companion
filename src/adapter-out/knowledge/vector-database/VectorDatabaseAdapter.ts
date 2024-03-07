import { ChatMessageDto } from "@/src/domain/models/Chats";
import { KnowledgeSummary } from "@/src/domain/models/DataSources";
import { MemoryManager } from "@/src/lib/memory";
import { getTokenLength } from "@/src/lib/tokenCount";

const MAX_DOCS_REQUESTED = 1000;
const MINIMUM_RELEVANCE_SCORE = 0.7;

export interface VectorKnowledgeResponse {
  knowledge: string;
  docMeta: Record<string, any>[];
  docsRequested: number;
  tokensReturned: number;
}

export class VectorDatabaseAdapter {
  public async getKnowledge(
    prompt: string,
    history: ChatMessageDto[],
    aiKnowledgeSummary: KnowledgeSummary,
    availTokens: number
  ): Promise<VectorKnowledgeResponse> {
    const { knowledgeIds, tokenCount, documentCount } = aiKnowledgeSummary;
    if (knowledgeIds.length === 0 || tokenCount === 0) {
      return {
        knowledge: "",
        docMeta: [],
        docsRequested: 0,
        tokensReturned: 0,
      };
    }

    const docDensity = tokenCount / documentCount;
    let estDocsNeeded = Math.ceil(availTokens / docDensity) || 100;
    estDocsNeeded = Math.min(MAX_DOCS_REQUESTED, Math.max(estDocsNeeded, 1));

    let query = prompt;
    for (let i = 2; i <= Math.min(3, history.length); i++) {
      query = `${history[history.length - i].content}\n${query}`;
    }

    const memoryManager = await MemoryManager.getInstance();
    const similarDocs = await memoryManager.vectorSearch(
      query,
      knowledgeIds,
      estDocsNeeded
    );

    let knowledge = "",
      docMeta = [];
    let totalTokenCount = 0;
    if (similarDocs) {
      for (const docWithRelevanceScore of similarDocs) {
        const doc = docWithRelevanceScore[0];
        const relevanceSore = docWithRelevanceScore[1];
        const documentTokens =
          doc.metadata.tokenCount || getTokenLength(doc.pageContent);

        if (
          documentTokens + totalTokenCount < availTokens &&
          relevanceSore > MINIMUM_RELEVANCE_SCORE
        ) {
          knowledge = `${knowledge}\n${doc.pageContent}`;
          totalTokenCount += documentTokens;
          docMeta.push(doc.metadata);
        } else {
          break;
        }
      }
    }
    return {
      knowledge,
      docMeta,
      docsRequested: estDocsNeeded,
      tokensReturned: totalTokenCount,
    };
  }

  public async vectorIdList(
    knowledgeId: string,
    paginationToken?: string
  ): Promise<{ vectorIds: string[]; paginationNextToken: string | undefined }> {
    const memoryManager = await MemoryManager.getInstance();
    return await memoryManager.vectorIdList(knowledgeId, paginationToken);
  }

  public async deleteVectors(vectorIds: string[]): Promise<void> {
    const memoryManager = await MemoryManager.getInstance();
    await memoryManager.vectorDelete(vectorIds);
  }
}

const vectorDatabaseAdapter = new VectorDatabaseAdapter();
export default vectorDatabaseAdapter;
