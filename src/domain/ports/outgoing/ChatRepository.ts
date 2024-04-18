import { Prisma } from "@prisma/client";
import {
  ChatDetailDto,
  ChatForWriteDto,
  ChatMessageDto,
} from "../../models/Chats";

export interface ChatRepository {
  findById(id: string): Promise<ChatDetailDto | null>;

  getById(id: string): Promise<ChatDetailDto>;

  getByIdForWrite(id: string): Promise<ChatForWriteDto>;

  createChat(chat: Prisma.ChatUncheckedCreateInput): Promise<ChatDetailDto>;

  addMessageToChat(
    id: string,
    orgId: string,
    userId: string,
    message: ChatMessageDto,
    externalChatId?: string
  ): Promise<ChatForWriteDto>;

  deleteChat(id: string): Promise<void>;
}
