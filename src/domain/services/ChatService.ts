import prismadb from "@/src/lib/prismadb";
import { GetChatsResponse } from "@/src/ports/api/ChatsApi";
import { Role } from "@prisma/client";

export class ChatService {
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

  public async updateChat(
    aiId: string,
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

    return chat;
  }
}

const chatService = new ChatService();
export default chatService;
