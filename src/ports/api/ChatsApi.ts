export interface GetChatsResponse {
  data: ChatDto[];
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
  prompt: string;
}
