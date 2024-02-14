import { Prisma } from "@prisma/client";
import { ChatDetailDto } from "../../models/Chats";

export interface ChatRepository {
  getById(id: string): Promise<ChatDetailDto>;
  findById(id: string): Promise<ChatDetailDto | null>;
  createChat(chat: Prisma.ChatUncheckedCreateInput): Promise<ChatDetailDto>;
  deleteChat(id: string): Promise<void>;
}
