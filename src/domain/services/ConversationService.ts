import prismadb from "@/src/lib/prismadb";
import { GetChatsResponse } from "@/src/ports/api/ChatsApi";
import { Role } from "@prisma/client";
import { EntityNotFoundError } from "../errors/Errors";

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

  public async updateConversation(
    aiId: string,
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
        aiId,
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
            aiId: aiId,
            metadata,
          },
        },
      },
    });

    return conversation;
  }

  private async getLatestConversationId(aiId: string, userId: string) {
    const ai = await prismadb.aI.findUnique({
      where: {
        id: aiId,
      },
      include: {
        conversations: {
          where: {
            userId: userId,
            isDeleted: false,
          },
          orderBy: {
            updatedAt: "desc",
          },
        },
      },
    });

    if (!ai) {
      throw new EntityNotFoundError(`AI with id ${aiId} not found`);
    }

    let conversation;
    if (ai.conversations.length === 0) {
      conversation = await prismadb.conversation.create({
        data: {
          aiId,
          name: ai.name,
          userId: userId,
        },
      });
    } else {
      conversation = ai.conversations[0];
    }

    return conversation.id;
  }
}

const conversationService = new ConversationService();
export default conversationService;
