import prismadb from "@/src/lib/prismadb";
import { GetChatsResponse } from "@/src/ports/api/ChatsApi";
import { Role } from "@prisma/client";
import { EntityNotFoundError } from "../errors/Errors";
import aiService from "./AIService";

export class ChatService {
  /**
   * Returns all chats for a given user
   * @param userId
   * @returns
   */
  public async getUserChats(userId: string): Promise<GetChatsResponse> {
    const conversations = await prismadb.chat.findMany({
      select: {
        id: true,
        createdAt: true,
        updatedAt: true,
        name: true,
        aiId: true,
        userId: true,
        pinPosition: true,
      },
      where: {
        userId,
        isDeleted: false,
      },
    });

    return {
      data: conversations,
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
      select: {
        id: true,
        createdAt: true,
        updatedAt: true,
        name: true,
        aiId: true,
        userId: true,
        pinPosition: true,
      },
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

    return await prismadb.chat.create({
      data: {
        aiId,
        userId,
        name: ai.name,
      },
    });
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
}

const chatService = new ChatService();
export default chatService;
