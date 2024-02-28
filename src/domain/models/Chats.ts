import { Role } from "@prisma/client";
import { AIModelOptions } from "./AIModel";

export interface ChatSummaryDto {
  id: string;
  createdAt?: Date;
  updatedAt?: Date;
  messagedAt?: Date | null;
  name: string;
  summary: string | null;
  orgId: string;
  userId: string;
  pinPosition: number | null;
  ai: ChatAiDto;
}

export interface ChatDetailDto extends ChatSummaryDto {
  messages: ChatMessageDto[];
}

export interface ChatForWriteDto extends ChatDetailDto {
  ai: ChatAiForWriteDto;
}

export interface ChatMessageDto {
  id?: string;
  createdAt?: Date;
  updatedAt?: Date;
  role: Role;
  content: string;
  metadata?: any;
}

export interface ChatAiDto {
  id: string;
  name: string;
  src: string;
  description: string;
  userId: string;
  userName: string;
}

export interface ChatAiForWriteDto extends ChatAiDto {
  modelId: string;
  options?: AIModelOptions;
  instructions?: string;
}
