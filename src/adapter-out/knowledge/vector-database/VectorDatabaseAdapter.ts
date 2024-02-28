import { ChatMessageDto } from "@/src/domain/models/Chats";
import { KnowledgeSummary } from "@/src/domain/models/DataSources";
import { MemoryManager } from "@/src/lib/memory";
import { getTokenLength } from "@/src/lib/tokenCount";

export interface VectorKnowledgeResponse {
  knowledge: string;
  docMeta: Record<string, any>[];
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
      return { knowledge: "", docMeta: [] };
    }

    const docDensity = tokenCount / documentCount;
    let estDocsNeeded = Math.ceil(availTokens / docDensity) || 100;
    estDocsNeeded = Math.min(10000, Math.max(estDocsNeeded, 1));

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
    if (!!similarDocs && similarDocs.length !== 0) {
      for (const doc of similarDocs) {
        const newKnowledge = `${knowledge}\n${doc.pageContent}`;
        const newKnowledgeTokens = getTokenLength(newKnowledge);
        if (newKnowledgeTokens < availTokens) {
          knowledge = newKnowledge;
          docMeta.push(doc.metadata);
        } else {
          break;
        }
      }
    }
    return { knowledge, docMeta };
  }

  public async deleteKnowledge(knowledgeId: string): Promise<void> {
    const memoryManager = await MemoryManager.getInstance();
    await memoryManager.vectorDelete(knowledgeId);
  }
}

const vectorDatabaseAdapter = new VectorDatabaseAdapter();
export default vectorDatabaseAdapter;
