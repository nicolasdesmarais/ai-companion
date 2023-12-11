import vectorDatabaseAdapter, {
  VectorKnowledgeResponse,
} from "@/src/adapter-out/knowledge/vector-database/VectorDatabaseAdapter";
import {
  ChatDetailDto,
  CreateChatRequest,
  ListChatsResponse,
} from "@/src/domain/ports/api/ChatsApi";
import prismadb from "@/src/lib/prismadb";
import { getTokenLength } from "@/src/lib/tokenCount";
import { AuthorizationContext } from "@/src/security/models/AuthorizationContext";
import { Prisma, Role } from "@prisma/client";
import { JsonObject } from "@prisma/client/runtime/library";
import axios from "axios";
import { ChatSecurityService } from "../../security/services/ChatSecurityService";
import { EntityNotFoundError, ForbiddenError } from "../errors/Errors";
import aiModelService from "./AIModelService";
import aiService from "./AIService";

const BUFFER_TOKENS = 200;

const listChatsResponseSelect: Prisma.ChatSelect = {
  id: true,
  createdAt: true,
  updatedAt: true,
  name: true,
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
      visibility: true,
    },
  },
};

const getChatResponseSelect: Prisma.ChatSelect = {
  ...listChatsResponseSelect,
  messages: {
    select: {
      id: true,
      createdAt: true,
      updatedAt: true,
      role: true,
      content: true,
    },
    orderBy: {
      createdAt: "asc",
    },
  },
};

export class ChatService {
  public async getChat(
    authorizationContext: AuthorizationContext,
    chatId: string
  ): Promise<ChatDetailDto> {
    const chat = await prismadb.chat.findUnique({
      select: getChatResponseSelect,
      where: {
        id: chatId,
        isDeleted: false,
      },
    });

    if (!chat) {
      throw new EntityNotFoundError(`Chat with id ${chatId} not found`);
    }

    const hasPermission = ChatSecurityService.canReadChat(
      authorizationContext,
      chat
    );
    if (!hasPermission) {
      throw new ForbiddenError("Forbidden");
    }

    return chat;
  }

  /**
   * Returns all chats for a given user
   * @param userId
   * @returns
   */
  public async getUserChats(userId: string): Promise<ListChatsResponse> {
    const chats = await prismadb.chat.findMany({
      select: listChatsResponseSelect,
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
    authorizationContext: AuthorizationContext,
    aiId: string
  ): Promise<ListChatsResponse> {
    const { userId } = authorizationContext;
    const chats = await prismadb.chat.findMany({
      select: listChatsResponseSelect,
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

  public async createChat(
    authorizationContext: AuthorizationContext,
    aiId: string
  ) {
    const ai = await aiService.findAIForUser(authorizationContext, aiId);
    if (!ai) {
      throw new EntityNotFoundError(`AI with id ${aiId} not found`);
    }

    const { orgId, userId } = authorizationContext;

    const chat = await prismadb.chat.create({
      data: {
        aiId,
        orgId,
        userId,
        name: ai.name,
      },
    });

    return chat;
  }

  public async updateChat(
    chatId: string,
    userId: string,
    content: string,
    role: Role,
    externalChatId?: string,
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
        externalId: externalChatId,
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

  public async getTestChat(
    aiId: string,
    userId: string,
    messages: any[],
    prompt: string
  ) {
    const ai = await prismadb.aI.findUnique({
      where: {
        id: aiId,
      },
      include: {
        dataSources: true,
      },
    });
    const chat = { ai, messages: messages || [] };
    chat.messages.push({
      content: prompt,
      role: Role.user,
      userId,
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

  public async postToChat(
    chatId: string,
    userId: string,
    request: CreateChatRequest
  ) {
    const { prompt, date, messages, aiId } = request;

    const start = performance.now();
    let endSetup = start,
      endKnowledge = start,
      knowledgeMeta: any,
      chat: any;

    if (chatId === "test-chat") {
      if (!aiId) {
        throw new Error("AI id not found");
      }
      chat = await this.getTestChat(aiId, userId, messages || [], prompt);
    } else {
      // Save prompt as a new message to chat
      chat = await this.updateChat(chatId, userId, request.prompt, Role.user);
    }

    if (!chat) {
      throw new EntityNotFoundError(`Chat with id ${chatId} not found`);
    }
    console.log("ai", chat);
    const model = await aiModelService.findAIModelById(chat.ai.modelId);
    if (!model) {
      throw new EntityNotFoundError(
        `AI model with id ${chat.ai.modelId} not found`
      );
    }

    let options = {} as any;
    Object.entries(chat.ai.options || {}).forEach(([key, value]) => {
      if (value && (value as any[]).length > 0) {
        options[key] = (value as any[])[0];
      }
    });

    const endCallback = async (answer: string, externalChatId?: string) => {
      if (chatId !== "test-chat") {
        const end = performance.now();
        const setupTime = Math.round(endSetup - start);
        const knowledgeTime = Math.round(endKnowledge - endSetup);
        const llmTime = Math.round(end - endKnowledge);
        const totalTime = Math.round(end - start);
        await this.updateChat(
          chatId,
          userId,
          answer,
          Role.system,
          externalChatId,
          {
            setupTime,
            knowledgeTime,
            llmTime,
            totalTime,
            knowledgeMeta,
          }
        );
      }
    };

    const chatModel = aiModelService.getChatModelInstance(model.id);
    if (!chatModel) {
      throw new Error(`Chat model with id ${model.id} not found`);
    }

    endSetup = performance.now();

    const getKnowledgeCallback = async (
      tokensUsed: number
    ): Promise<VectorKnowledgeResponse> => {
      let bootstrapKnowledge;
      if (chat.ai.dataSources.length === 1) {
        if (chat.ai.dataSources[0].dataSource.knowledges.length === 1) {
          const meta = chat.ai.dataSources[0].dataSource.knowledges[0].knowledge
            .metadata as any;
          if (
            meta &&
            meta.mimeType &&
            meta.totalTokenCount &&
            meta.mimeType === "text/plain"
          ) {
            bootstrapKnowledge = {
              ...chat.ai.dataSources[0].dataSource.knowledges[0].knowledge,
              ...meta,
            };
          }
        }
      }

      const questionTokens = getTokenLength(prompt);
      const answerTokens = ((chat.ai.options as JsonObject)?.maxTokens ||
        model.options.maxTokens.default) as number;

      const remainingTokens =
        model.contextSize -
        answerTokens -
        questionTokens -
        tokensUsed -
        BUFFER_TOKENS;

      let knowledgeResponse: VectorKnowledgeResponse = {
        knowledge: "",
        docMeta: [],
      };
      if (
        bootstrapKnowledge?.blobUrl &&
        remainingTokens > bootstrapKnowledge.totalTokenCount
      ) {
        const resp = await axios.get(bootstrapKnowledge.blobUrl);
        if (resp.status === 200) {
          knowledgeResponse.knowledge = resp.data;
        }
      }
      if (!knowledgeResponse.knowledge) {
        const vectorKnowledge = await vectorDatabaseAdapter.getKnowledge(
          prompt,
          chat.messages,
          chat.ai.dataSources,
          remainingTokens
        );
        knowledgeResponse.knowledge = vectorKnowledge.knowledge;
      }

      endKnowledge = performance.now();
      return knowledgeResponse;
    };

    return await chatModel.postToChat({
      ai: chat.ai,
      chat,
      messages: chat.messages,
      aiModel: model,
      prompt,
      date,
      options,
      getKnowledgeCallback,
      endCallback,
    });
  }
}

const chatService = new ChatService();
export default chatService;
