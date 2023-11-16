export interface GetChatsResponse {
  chats: ChatDto[];
}

export interface ChatDto {
  id: string;
  createdAt: Date;
  updatedAt: Date;
  name: string;
  aiId: string;
  userId: string;
  pinPosition: number | null;
}

export interface CreateChatRequest {
  date: string;
  conversationId?: string;
  prompt: string;
}
