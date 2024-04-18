import { ChatMessageDto, ChatSummaryDto } from "@/src/domain/models/Chats";

export interface ListChatsResponse {
  data: ChatSummaryDto[];
}
export interface CreateChatRequest {
  date: string;
  prompt: string;
  aiId?: string;
  messages?: ChatMessageDto[];
}
