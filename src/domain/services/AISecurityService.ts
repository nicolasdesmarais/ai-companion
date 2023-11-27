import { AI } from "@prisma/client";

export class AISecurityService {
  public static canUpdateAI(orgId: string, userId: string, ai: AI) {
    return ai.orgId === orgId && ai.userId === userId;
  }
}
