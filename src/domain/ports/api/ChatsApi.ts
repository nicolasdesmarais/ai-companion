export interface ListChatsResponse {
  data: ChatListDto[];
}

export interface ChatListDto {
  id: string;
  createdAt: Date;
  updatedAt: Date;
  name: string;
  userId: string;
  pinPosition: number | null;
  ai: ChatAiDto;
}

export interface ChatAiDto {
  id: string;
  name: string;
  src: string;
  description: string;
}

export interface CreateChatRequest {
  date: string;
  prompt: string;
  aiId?: string;
  messages?: any[];
}
