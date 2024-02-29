import { AIModel } from "@/src/domain/models/AIModel";
import { ChatForWriteDto, ChatMessageDto } from "@/src/domain/models/Chats";
import { ChatCallbackContext } from "@/src/domain/services/ChatService";
import { VectorKnowledgeResponse } from "../../knowledge/vector-database/VectorDatabaseAdapter";

export interface ChatModel {
  supports(model: AIModel): boolean;

  postToChat(input: PostToChatInput): Promise<PostToChatResponse>;
}

export interface PostToChatInput {
  chat: ChatForWriteDto;
  messages: ChatMessageDto[];
  aiModel: AIModel;
  prompt: string;
  date: string;
  options: any;
  callbackContext: ChatCallbackContext;
  getKnowledgeCallback: (
    input: PostToChatInput,
    tokensUsed: number
  ) => Promise<VectorKnowledgeResponse>;
  startChatCallback: (callbackContext: ChatCallbackContext) => void;
  endChatCallback: (
    callbackContext: ChatCallbackContext,
    answer: string,
    externalChatId?: string
  ) => Promise<void>;
}

export interface PostToChatResponse {
  isStream: boolean;
  response: string | ReadableStream<any>;
}
