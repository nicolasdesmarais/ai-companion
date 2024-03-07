import {
  CreateChatRequest,
  ListChatsResponse,
} from "@/src/adapter-in/api/ChatsApi";
import { gpt4ChatModel } from "@/src/adapter-out/ai-model/chat-models/Gpt4Model";
import vectorDatabaseAdapter, {
  VectorKnowledgeResponse,
} from "@/src/adapter-out/knowledge/vector-database/VectorDatabaseAdapter";

import { PostToChatInput } from "@/src/adapter-out/ai-model/chat-models/ChatModel";
import { ChatRepositoryImpl } from "@/src/adapter-out/repositories/ChatRepositoryImpl";
import {
  ChatDetailDto,
  ChatForWriteDto,
  ChatMessageDto,
} from "@/src/domain/models/Chats";
import prismadb from "@/src/lib/prismadb";
import { getTokenLength } from "@/src/lib/tokenCount";
import { AuthorizationContext } from "@/src/security/models/AuthorizationContext";
import { SystemMessage } from "@langchain/core/messages";
import { Prisma, Role } from "@prisma/client";
import { ChatSecurityService } from "../../security/services/ChatSecurityService";
import {
  BadRequestError,
  EntityNotFoundError,
  ForbiddenError,
} from "../errors/Errors";
import { ChatRepository } from "../ports/outgoing/ChatRepository";
import aiModelService from "./AIModelService";
import aiService from "./AIService";
import knowledgeService from "./KnowledgeService";

const BUFFER_TOKENS = 200;

const listChatsResponseSelect: Prisma.ChatSelect = {
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

const getChatResponseSelect: Prisma.ChatSelect = {
  ...listChatsResponseSelect,
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

export interface ChatCallbackContext {
  chatId: string;
  orgId: string;
  userId: string;
  start: number;
  endSetup: number;
  endKnowledge: number;
  hasChatStarted: boolean;
  startChat: number;
  recordedTokensUsed: number;
  knowledgeDocumentsRequested: number;
  knowledgeTokensReturned: number;
  knowledgeMeta?: any;
}

export class ChatService {
  constructor(private chatRepository: ChatRepository) {}

  public async getChat(
    authorizationContext: AuthorizationContext,
    chatId: string
  ): Promise<ChatDetailDto> {
    const chat = await this.chatRepository.getById(chatId);

    const hasPermission = ChatSecurityService.canReadChat(
      authorizationContext,
      chat
    );
    if (!hasPermission) {
      throw new ForbiddenError("Forbidden");
    }

    return chat;
  }

  private async getChatForWrite(
    authorizationContext: AuthorizationContext,
    chatId: string
  ): Promise<ChatForWriteDto> {
    const chat = await this.chatRepository.getByIdForWrite(chatId);

    const hasPermission = ChatSecurityService.canWriteChat(
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
  public async getUserChats(
    userId: string,
    orgId: string
  ): Promise<ListChatsResponse> {
    const chats = await prismadb.chat.findMany({
      select: listChatsResponseSelect,
      where: {
        orgId,
        userId,
        isDeleted: false,
      },
      orderBy: [
        {
          messagedAt: "desc",
        },
        {
          updatedAt: "desc",
        },
      ],
    });

    return {
      data: chats,
    };
  }

  /**
   * Returns all chats for a given AI and user
   * @param aiId
   * @param userId
   * @returns
   */
  public async getAIChats(
    authorizationContext: AuthorizationContext,
    aiId: string
  ): Promise<ListChatsResponse> {
    const { orgId, userId } = authorizationContext;
    const chats = await prismadb.chat.findMany({
      select: listChatsResponseSelect,
      where: {
        aiId,
        orgId,
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
      throw new BadRequestError(`AI with id ${aiId} not found`);
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

  public async getTestChat(
    chatId: string,
    aiId: string,
    orgId: string,
    userId: string,
    messages: ChatMessageDto[],
    prompt: string
  ): Promise<ChatForWriteDto> {
    const ai = await prismadb.aI.findUnique({
      where: {
        id: aiId,
      },
    });
    const chat = {
      id: chatId,
      orgId,
      userId,
      messages: messages || [],
      name: "Test Chat",
      summary: "",
      pinPosition: null,
      ai: {
        id: aiId,
        name: ai?.name || "Test AI",
        src: "",
        description: ai?.description || "",
        userId: ai?.userId || "",
        userName: "",
        modelId: ai?.modelId,
      },
    };
    chat.messages.push({
      content: prompt,
      role: Role.user,
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
    authorizationContext: AuthorizationContext,
    chatId: string,
    request: CreateChatRequest
  ) {
    const start = performance.now();

    const { prompt, date } = request;
    const { orgId, userId } = authorizationContext;

    const chatCallbackContext: ChatCallbackContext = {
      chatId,
      orgId,
      userId,
      start,
      endSetup: start,
      endKnowledge: start,
      hasChatStarted: false,
      startChat: start,
      recordedTokensUsed: 0,
      knowledgeDocumentsRequested: 0,
      knowledgeTokensReturned: 0,
    };

    const chat = await this.retrieveChatAndAddMessage(
      authorizationContext,
      request,
      chatId
    );

    const aiModel = await aiModelService.getAIModelById(chat.ai.modelId);

    let options = {} as any;
    Object.entries(chat.ai.options || {}).forEach(([key, value]) => {
      if (value && (value as any[]).length > 0) {
        options[key] = (value as any[])[0];
      }
    });

    const chatModel = aiModelService.getChatModelInstance(aiModel);
    if (!chatModel) {
      throw new Error(`Chat model with id ${aiModel.id} not found`);
    }

    chatCallbackContext.endSetup = performance.now();

    //TODO: fix timeout for long chat history
    let prunedMessages = chat.messages;
    if (prunedMessages?.length > 60) {
      prunedMessages = prunedMessages.slice(prunedMessages.length - 60);
    }
    return await chatModel.postToChat({
      chat,
      messages: prunedMessages,
      aiModel,
      prompt,
      date,
      options,
      callbackContext: chatCallbackContext,
      getKnowledgeCallback: this.getKnowledgeCallback,
      startChatCallback: this.startChatCallback,
      endChatCallback: this.endChatCallback,
    });
  }

  private async retrieveChatAndAddMessage(
    authorizationContext: AuthorizationContext,
    request: CreateChatRequest,
    chatId: string
  ): Promise<ChatForWriteDto> {
    const { prompt, messages, aiId } = request;
    const { orgId, userId } = authorizationContext;

    let chat: ChatForWriteDto;
    if (chatId === "test-chat") {
      if (!aiId) {
        throw new Error("AI id not found");
      }
      chat = await this.getTestChat(
        chatId,
        aiId,
        orgId,
        userId,
        messages || [],
        prompt
      );
    } else {
      // Save prompt as a new message to chat
      const message: ChatMessageDto = {
        role: Role.user,
        content: prompt,
      };

      chat = await this.chatRepository.addMessageToChat(
        chatId,
        orgId,
        userId,
        message
      );

      if (!chat) {
        throw new EntityNotFoundError(
          `Chat with id=${chatId} not found for org=${orgId} and user=${userId}`
        );
      }
    }
    return chat;
  }

  private startChatCallback(context: ChatCallbackContext): void {
    if (!context.hasChatStarted) {
      context.startChat = performance.now();
      context.hasChatStarted = true;
    }
  }

  private async endChatCallback(
    context: ChatCallbackContext,
    answer: string,
    externalChatId?: string
  ): Promise<void> {
    const {
      chatId,
      orgId,
      userId,
      start,
      endSetup,
      endKnowledge,
      startChat,
      recordedTokensUsed,
      knowledgeDocumentsRequested,
      knowledgeTokensReturned,
      knowledgeMeta,
    } = context;

    if (chatId === "test-chat") {
      return;
    }

    const end = performance.now();
    const setupTime = Math.round(endSetup - start);
    const knowledgeTime = Math.round(endKnowledge - endSetup);
    const startLlmTime = Math.round(startChat - endKnowledge);
    const llmTime = Math.round(end - endKnowledge);
    const totalTime = Math.round(end - start);

    const message: ChatMessageDto = {
      role: Role.system,
      content: answer,
      metadata: {
        setupTime,
        knowledgeTime,
        startLlmTime,
        llmTime,
        totalTime,
        knowledgeMeta,
        tokensUsed: recordedTokensUsed,
        knowledgeDocumentsRequested: knowledgeDocumentsRequested,
        knowledgeTokensReturned: knowledgeTokensReturned,
      },
    };

    await chatRepository.addMessageToChat(
      chatId,
      orgId,
      userId,
      message,
      externalChatId
    );
  }

  private async getKnowledgeCallback(
    input: PostToChatInput,
    tokensUsed: number
  ): Promise<VectorKnowledgeResponse> {
    const { chat, messages, aiModel, prompt, callbackContext } = input;
    const { ai } = chat;

    const answerTokens = (ai.options?.maxTokens ??
      aiModel.options.maxTokens.default) as number;

    const remainingTokens =
      aiModel.contextSize - answerTokens - tokensUsed - BUFFER_TOKENS;

    const knowledgeSummary = await knowledgeService.getAiKnowledgeSummary(
      ai.id
    );

    const vectorKnowledge = await vectorDatabaseAdapter.getKnowledge(
      prompt,
      messages,
      knowledgeSummary,
      remainingTokens
    );

    callbackContext.endKnowledge = performance.now();
    callbackContext.recordedTokensUsed = tokensUsed;
    callbackContext.knowledgeDocumentsRequested = vectorKnowledge.docsRequested;
    callbackContext.knowledgeTokensReturned = vectorKnowledge.tokensReturned;
    return vectorKnowledge;
  }

  public async getKnowledge(
    authorizationContext: AuthorizationContext,
    request: CreateChatRequest,
    tokensUsed: number
  ) {
    const { orgId, userId } = authorizationContext;
    const { prompt, aiId } = request;

    const messages = request.messages || [];

    if (!aiId) {
      throw new Error("AI id not found");
    }

    const chat = await this.getTestChat(
      "test-chat",
      aiId,
      orgId,
      userId,
      messages || [],
      prompt
    );

    if (!chat?.ai) {
      throw new EntityNotFoundError(`AI with id ${aiId} not found`);
    }
    const model = await aiModelService.findAIModelById(chat.ai.modelId);
    if (!model) {
      throw new EntityNotFoundError(
        `AI model with id ${chat.ai.modelId} not found`
      );
    }
    const questionTokens = getTokenLength(prompt);
    const answerTokens = (chat.ai.options?.maxTokens ||
      model.options.maxTokens.default) as number;

    const remainingTokens =
      model.contextSize -
      answerTokens -
      questionTokens -
      tokensUsed -
      BUFFER_TOKENS;

    const knowledgeSummary = await knowledgeService.getAiKnowledgeSummary(
      chat.ai.id
    );

    const vectorKnowledge = await vectorDatabaseAdapter.getKnowledge(
      prompt,
      messages,
      knowledgeSummary,
      remainingTokens
    );
    return vectorKnowledge;
  }

  public async summarizeChat(chatId: string) {
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

    if (chat.messages.length === 0) {
      return;
    }

    const messages = chat.messages
      .map((message) => {
        if (message.role === Role.system) {
          return `${chat.ai.name}: ${message.content}\n`;
        } else {
          return `You: ${message.content}\n`;
        }
      })
      .join(" ");
    const resp = await gpt4ChatModel.invoke([
      new SystemMessage(
        `Describe the following conversation in under ten words. This will be displayed to the user, so refer to the user in second person singular. \n${messages}`
      ),
    ]);
    await prismadb.chat.update({
      where: {
        id: chatId,
      },
      data: {
        summary: resp.content as string,
      },
    });

    return resp.content;
  }

  public async resetChat(
    authorizationContext: AuthorizationContext,
    chatId: string
  ) {
    const chat = await this.getChatForWrite(authorizationContext, chatId);

    await this.chatRepository.deleteChat(chatId);

    return await this.chatRepository.createChat({
      orgId: chat.orgId,
      userId: chat.userId,
      name: chat.name,
      aiId: chat.ai.id,
      pinPosition: chat.pinPosition,
    });
  }
}

const chatRepository = new ChatRepositoryImpl();
const chatService = new ChatService(chatRepository);
export default chatService;
