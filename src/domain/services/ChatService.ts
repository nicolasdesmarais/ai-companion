import openAIAssistantModelAdapter from "@/src/adapter/ai-model/OpenAIAssistantModelAdapter";
import vectorDatabaseAdapter, {
  VectorKnowledgeResponse,
} from "@/src/adapter/knowledge/vector-database/VectorDatabaseAdapter";
import {
  CreateChatRequest,
  GetChatsResponse,
} from "@/src/domain/ports/api/ChatsApi";
import prismadb from "@/src/lib/prismadb";
import { getTokenLength } from "@/src/lib/tokenCount";
import { Role } from "@prisma/client";
import { JsonObject } from "@prisma/client/runtime/library";
import axios from "axios";
import { EntityNotFoundError, ForbiddenError } from "../errors/Errors";
import aiModelService from "./AIModelService";
import aiService from "./AIService";

const BUFFER_TOKENS = 200;

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

  public async postToChat(
    chatId: string,
    userId: string,
    request: CreateChatRequest
  ) {
    const { prompt, date } = request;

    const start = performance.now();
    let endSetup = start,
      endKnowledge = start,
      knowledgeMeta: any;

    // Save prompt as a new message to chat
    const chat = await this.updateChat(
      chatId,
      userId,
      request.prompt,
      Role.user
    );

    if (!chat) {
      throw new EntityNotFoundError(`Chat with id ${chatId} not found`);
    }

    const model = await aiModelService.findAIModelById(chat.ai.modelId);
    if (!model) {
      throw new EntityNotFoundError(
        `AI model with id ${chat.ai.modelId} not found`
      );
    }

    let options = {} as any;
    Object.entries(chat.ai.options || {}).forEach(([key, value]) => {
      if (value && value.length > 0) {
        options[key] = value[0];
      }
    });

    const endCallback = async (answer: string) => {
      const end = performance.now();
      const setupTime = Math.round(endSetup - start);
      const knowledgeTime = Math.round(endKnowledge - endSetup);
      const llmTime = Math.round(end - endKnowledge);
      const totalTime = Math.round(end - start);
      await this.updateChat(chatId, userId, answer, Role.system, {
        setupTime,
        knowledgeTime,
        llmTime,
        totalTime,
        knowledgeMeta,
      });
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
        bootstrapKnowledge &&
        bootstrapKnowledge.blobUrl &&
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
