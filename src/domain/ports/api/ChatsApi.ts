export interface ListChatsResponse {
  data: ChatSummaryDto[];
}

export interface ChatSummaryDto {
  id: string;
  createdAt: Date;
  updatedAt: Date;
  name: string;
  userId: string;
  pinPosition: number | null;
  ai: ChatAiDto;
}

export interface ChatDetailDto extends ChatSummaryDto {
  messages: ChatMessageDto[];
}

export interface ChatMessageDto {
  id: string;
  createdAt: Date;
  updatedAt: Date;
  role: string;
  content: string;
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
