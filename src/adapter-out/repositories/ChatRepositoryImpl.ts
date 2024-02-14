import { EntityNotFoundError } from "@/src/domain/errors/Errors";
import { ChatDetailDto } from "@/src/domain/models/Chats";
import { ChatRepository } from "@/src/domain/ports/outgoing/ChatRepository";
import prismadb from "@/src/lib/prismadb";
import { Prisma } from "@prisma/client";

const chatSummarySelect: Prisma.ChatSelect = {
  id: true,
  createdAt: true,
  updatedAt: true,
  name: true,
  summary: true,
  orgId: true,
  userId: true,
  pinPosition: true,
  ai: {
    select: {
      id: true,
      name: true,
      src: true,
      description: true,
      userId: true,
      userName: true,
    },
  },
};

const chatDetailSelect: Prisma.ChatSelect = {
  ...chatSummarySelect,
  messages: {
    select: {
      id: true,
      createdAt: true,
      updatedAt: true,
      role: true,
      content: true,
      metadata: true,
    },
    orderBy: {
      createdAt: "asc",
    },
  },
};

export class ChatRepositoryImpl implements ChatRepository {
  public async getById(id: string): Promise<ChatDetailDto> {
    const chat = await this.findById(id);
    if (!chat) {
      throw new EntityNotFoundError(`Chat with id ${id} not found`);
    }
    return chat;
  }

  public async findById(id: string): Promise<ChatDetailDto | null> {
    const chat = await prismadb.chat.findUnique({
      select: chatDetailSelect,
      where: {
        id,
      },
    });

    return chat;
  }

  public async createChat(
    chat: Prisma.ChatUncheckedCreateInput
  ): Promise<ChatDetailDto> {
    return await prismadb.chat.create({
      select: chatDetailSelect,
      data: chat,
    });
  }

  public async deleteChat(id: string): Promise<void> {
    await prismadb.chat.update({
      where: {
        id,
      },
      data: {
        isDeleted: true,
      },
    });
  }
}
