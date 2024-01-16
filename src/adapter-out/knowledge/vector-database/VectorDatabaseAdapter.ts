import { MemoryManager } from "@/src/lib/memory";
import { getTokenLength } from "@/src/lib/tokenCount";
import { Message } from "@prisma/client";

export interface VectorKnowledgeResponse {
  knowledge: string;
  docMeta: Record<string, any>[];
}

export class VectorDatabaseAdapter {
  public async getKnowledge(
    prompt: string,
    history: Message[],
    dataSources: any[],
    availTokens: number
  ): Promise<VectorKnowledgeResponse> {
    if (dataSources.length === 0) {
      return { knowledge: "", docMeta: [] };
    }

    const knowledgeIds: string[] = dataSources
      .map((ds) => ds.dataSource.knowledges.map((k: any) => k.knowledgeId))
      .reduce((acc, curr) => acc.concat(curr), []);

    const { totalDocs, totalTokens } = dataSources.reduce(
      (dsAcc, ds) => {
        const { docs, tokens } = ds.dataSource.knowledges.reduce(
          (acc: any, k: any) => {
            if (
              k.knowledge.metadata &&
              k.knowledge.metadata.totalTokenCount &&
              k.knowledge.metadata.documentCount
            ) {
              acc.tokens += k.knowledge.metadata.totalTokenCount;
              acc.docs += k.knowledge.metadata.documentCount;
              return acc;
            } else {
              return { docs: NaN, tokens: NaN };
            }
          },
          { docs: 0, tokens: 0 }
        );
        dsAcc.totalDocs += docs;
        dsAcc.totalTokens += tokens;
        return dsAcc;
      },
      { totalDocs: 0, totalTokens: 0 }
    );
    const docDensity = totalTokens / totalDocs;
    let estDocsNeeded = Math.ceil(availTokens / docDensity) || 100;
    estDocsNeeded = Math.min(10000, Math.max(estDocsNeeded, 1));

    let query = prompt;
    if (history.length > 1) {
      query = `${history[history.length - 2].content}\n${query}`;
    }
    if (history.length > 2) {
      query = `${history[history.length - 3].content}\n${query}`;
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
      for (let i = 0; i < similarDocs.length; i++) {
        const doc = similarDocs[i];
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
