import { AIModel } from "@/src/domain/models/AIModel";
import { AI, Chat, Message } from "@prisma/client";

export interface ChatModel {
  supports(modelId: string): boolean;

  postToChat(input: PostToChatInput): Promise<any>;
}

export interface PostToChatInput {
  ai: AI;
  chat: Chat;
  messages: Message[];
  aiModel: AIModel;
  prompt: string;
  date: string;
  answerTokens: number;
  questionTokens: number;
  bootstrapKnowledge: any;
  options: any;
  endCallback: (answer: string) => void;
}
