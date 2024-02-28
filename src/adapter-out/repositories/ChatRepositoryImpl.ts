import { EntityNotFoundError } from "@/src/domain/errors/Errors";
import { AIModelOptions } from "@/src/domain/models/AIModel";
import {
  ChatDetailDto,
  ChatForWriteDto,
  ChatMessageDto,
} from "@/src/domain/models/Chats";
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

const chatForWriteSelect: Prisma.ChatSelect = {
  ...chatDetailSelect,
  externalId: true,
  ai: {
    select: {
      id: true,
      name: true,
      src: true,
      description: true,
      userId: true,
      userName: true,
      modelId: true,
      options: true,
      instructions: true,
      externalId: true,
      seed: true,
    },
  },
};

const mapToChatForWriteDto = (chat: any) => {
  return {
    ...chat,
    ai: {
      ...chat.ai,
      options: chat.ai.options as unknown as AIModelOptions,
    },
  };
};

export class ChatRepositoryImpl implements ChatRepository {
  public async findById(id: string): Promise<ChatDetailDto | null> {
    const chat = await prismadb.chat.findUnique({
      select: chatDetailSelect,
      where: {
        id,
      },
    });

    return chat;
  }

  public async getById(id: string): Promise<ChatDetailDto> {
    const chat = await this.findById(id);
    if (!chat) {
      throw new EntityNotFoundError(`Chat with id ${id} not found`);
    }
    return chat;
  }

  public async getByIdForWrite(id: string): Promise<ChatForWriteDto> {
    const chat = await prismadb.chat.findUnique({
      select: chatForWriteSelect,
      where: {
        id,
      },
    });

    if (!chat) {
      throw new EntityNotFoundError(`Chat with id ${id} not found`);
    }
    return mapToChatForWriteDto(chat);
  }

  public async createChat(
    chat: Prisma.ChatUncheckedCreateInput
  ): Promise<ChatDetailDto> {
    return await prismadb.chat.create({
      select: chatDetailSelect,
      data: chat,
    });
  }

  public async addMessageToChat(
    id: string,
    orgId: string,
    userId: string,
    message: ChatMessageDto,
    externalChatId?: string
  ): Promise<ChatForWriteDto> {
    const updatedChat = await prismadb.chat.update({
      select: chatForWriteSelect,
      where: {
        id,
        orgId,
        userId,
      },
      data: {
        messagedAt: new Date(),
        externalId: externalChatId,
        messages: {
          create: {
            content: message.content,
            role: message.role,
            userId,
            metadata: message.metadata,
          },
        },
      },
    });

    return mapToChatForWriteDto(updatedChat);
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
