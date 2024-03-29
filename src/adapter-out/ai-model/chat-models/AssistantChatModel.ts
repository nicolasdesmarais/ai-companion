import { AIModel } from "@/src/domain/models/AIModel";
import { AI } from "@prisma/client";

export interface AssistantChatModel {
  supports(model: AIModel): boolean;

  createAssistant(input: CreateAssistantInput): Promise<string>;

  updateAssistant(input: UpdateAssistantInput): Promise<void>;

  deleteAssistant(externalId: string): Promise<void>;
}

export interface CreateAssistantInput {
  ai: AI;
}

export interface UpdateAssistantInput {
  ai: AI;
}
