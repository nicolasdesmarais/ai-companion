import openAIAssistantModelAdapter from "@/src/adapter/ai-model/OpenAIAssistantModelAdapter";
import { GetChatsResponse } from "@/src/domain/ports/api/ChatsApi";
import prismadb from "@/src/lib/prismadb";
import { Role } from "@prisma/client";
import { EntityNotFoundError, ForbiddenError } from "../errors/Errors";
import aiService from "./AIService";

const getChatsResponseSelect = {
  id: true,
  createdAt: true,
  updatedAt: true,
  name: true,
  aiId: true,
  userId: true,
  pinPosition: true,
  ai: {
    select: {
      id: true,
      name: true,
      src: true,
      description: true,
    },
  },
};

export class ChatService {
  /**
   * Returns all chats for a given user
   * @param userId
   * @returns
   */
  public async getUserChats(userId: string): Promise<GetChatsResponse> {
    const chats = await prismadb.chat.findMany({
      select: getChatsResponseSelect,
      where: {
        userId,
        isDeleted: false,
      },
    });

    return {
      data: chats,
    };
  }

  /**
   * Returns all chats for a given AI
   * @param aiId
   * @param userId
   * @returns
   */
  public async getAIChats(
    aiId: string,
    userId: string
  ): Promise<GetChatsResponse> {
    const chats = await prismadb.chat.findMany({
      select: getChatsResponseSelect,
      where: {
        aiId,
        userId,
        isDeleted: false,
      },
    });

    return {
      data: chats,
    };
  }

  public async createChat(orgId: string, userId: string, aiId: string) {
    const ai = await aiService.findAIForUser(orgId, userId, aiId);
    if (!ai) {
      throw new EntityNotFoundError(`AI with id ${aiId} not found`);
    }

    let externalId;
    if (ai.modelId === "gpt-4-1106-preview-assistant") {
      externalId = await openAIAssistantModelAdapter.createExternalChat();
    }

    const chat = await prismadb.chat.create({
      data: {
        aiId,
        userId,
        name: ai.name,
        externalId,
      },
    });

    return chat;
  }

  public async updateChat(
    chatId: string,
    userId: string,
    content: string,
    role: Role,

    metadata?: any
  ) {
    const chat = await prismadb.chat.update({
      where: {
        id: chatId,
        userId,
      },
      include: {
        ai: {
          include: {
            dataSources: {
              include: {
                dataSource: {
                  include: {
                    knowledges: {
                      include: {
                        knowledge: true,
                      },
                    },
                  },
                },
              },
            },
          },
        },
        messages: {
          orderBy: {
            createdAt: "asc",
          },
        },
      },
      data: {
        messages: {
          create: {
            content: content,
            role,
            userId,
            metadata,
          },
        },
      },
    });

    return chat;
  }

  public async deleteChat(chatId: string, userId: string) {
    const chat = await prismadb.chat.findUnique({
      where: {
        id: chatId,
      },
    });

    if (!chat) {
      throw new EntityNotFoundError(`Chat with id ${chatId} not found`);
    }

    if (chat.userId !== userId) {
      throw new ForbiddenError("Forbidden");
    }

    await prismadb.chat.update({
      where: {
        id: chatId,
      },
      data: {
        isDeleted: true,
      },
    });
  }
}

const chatService = new ChatService();
export default chatService;
