import { AIRequest } from "@/src/adapter-in/api/AIApi";
import { AIModel } from "@/src/domain/models/AIModel";

export interface AssistantChatModel {
  supports(model: AIModel): boolean;

  createAssistant(input: CreateAssistantInput): Promise<string>;

  updateAssistant(input: UpdateAssistantInput): Promise<void>;

  deleteAssistant(externalId: string): Promise<void>;
}

export interface CreateAssistantInput {
  ai: AIRequest;
}

export interface UpdateAssistantInput {
  assistantId: string;
  ai: AIRequest;
}
