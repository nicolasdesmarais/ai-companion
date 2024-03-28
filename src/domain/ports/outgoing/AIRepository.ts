import { AI } from "@prisma/client";

export interface AIRepository {
  getById(id: string): Promise<AI>;
  hasPermissionOnAI(aiId: string, userId: string): Promise<boolean>;
  approveAIForOrg(aiId: string, orgId: string): Promise<void>;
  revokeAIForOrg(aiId: string, orgId: string): Promise<void>;
}
