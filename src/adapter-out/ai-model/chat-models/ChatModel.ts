import { AIModel } from "@/src/domain/models/AIModel";
import { AI, Chat, Message } from "@prisma/client";
import { VectorKnowledgeResponse } from "../../knowledge/vector-database/VectorDatabaseAdapter";

export interface ChatModel {
  supports(model: AIModel): boolean;

  postToChat(input: PostToChatInput): Promise<PostToChatResponse>;
}

export interface PostToChatInput {
  ai: AI;
  chat: Chat;
  messages: Message[];
  aiModel: AIModel;
  prompt: string;
  date: string;
  options: any;
  getKnowledgeCallback: (
    tokensUsed: number
  ) => Promise<VectorKnowledgeResponse>;
  endCallback: (answer: string, externalChatId?: string) => void;
}

export interface PostToChatResponse {
  isStream: boolean;
  response: string | ReadableStream<any>;
}
