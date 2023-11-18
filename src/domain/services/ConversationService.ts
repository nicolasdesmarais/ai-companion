import prismadb from "@/src/lib/prismadb";
import { GetChatsResponse } from "@/src/ports/api/ChatsApi";
import { Role } from "@prisma/client";
import { EntityNotFoundError } from "../errors/Errors";
import aiService from "./AIService";

export class ConversationService {
  public async getAIConversations(
    aiId: string,
    userId: string
  ): Promise<GetChatsResponse> {
    const conversations = await prismadb.conversation.findMany({
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
      data: conversations,
    };
  }

  public async getUserConversations(userId: string): Promise<GetChatsResponse> {
    const conversations = await prismadb.conversation.findMany({
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

  public async createConversation(orgId: string, userId: string, aiId: string) {
    const ai = await aiService.findAIForUser(orgId, userId, aiId);
    if (!ai) {
      throw new EntityNotFoundError(`AI with id ${aiId} not found`);
    }

    return await prismadb.conversation.create({
      data: {
        aiId,
        userId,
        name: ai.name,
      },
    });
  }

  public async updateConversation(
    conversationId: string,
    userId: string,
    content: string,
    role: Role,

    metadata?: any
  ) {
    const conversation = await prismadb.conversation.update({
      where: {
        id: conversationId,
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

    return conversation;
  }
}

const conversationService = new ConversationService();
export default conversationService;
