import { EntityNotFoundError } from "@/src/domain/errors/Errors";
import { AIModel } from "@/src/domain/models/AIModel";
import aiModelService from "@/src/domain/services/AIModelService";
import chatService from "@/src/domain/services/ChatService";
import prismadb from "@/src/lib/prismadb";
import { getTokenLength } from "@/src/lib/tokenCount";
import { AI, Role } from "@prisma/client";
import { JsonObject } from "@prisma/client/runtime/library";
import { OpenAIAssistantRunnable } from "langchain/experimental/openai_assistant";
import OpenAI from "openai";
import { ThreadMessage } from "openai/resources/beta/threads/messages/messages.mjs";

const openai = new OpenAI({
  apiKey: process.env.OPENAI_API_KEY,
});

const BUFFER_TOKENS = 1000;

export class OpenAIAssistantModelAdapter {
  public async createExternalAI(ai: AI, aiModel: AIModel): Promise<string> {
    const lca = await OpenAIAssistantRunnable.createAssistant({
      model: "gpt-4-1106-preview",
    });

    const instructions = `
        Pretend you are ${ai.name}, ${ai.description}.
        Output format is markdown. Open links in new tabs.
        Here are more details about your character:\n
        ${ai.instructions}

        Below are relevant details about ${ai.name}'s past:\n
        ${ai.seed}
      `;

    const assistant = await openai.beta.assistants.create({
      model: aiModel.externalModelId,
      name: ai.name,
      description: ai.description,
      instructions: instructions,

      tools: [
        {
          type: "code_interpreter",
        },
      ],
    });

    return assistant.id;
  }

  public async createExternalChat() {
    const thread = await openai.beta.threads.create();
    return thread.id;
  }

  public async postToChat(chatId: string, userId: string, prompt: string) {
    const chat = await prismadb.chat.findUnique({
      where: {
        id: chatId,
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
    });
    if (!chat) {
      throw new EntityNotFoundError("Chat not found");
    }
    const ai = chat.ai;
    if (!ai.externalId) {
      throw new Error("External AI not found");
    }

    const aiModel = await aiModelService.findAIModelById(ai.modelId);
    if (!aiModel) {
      throw new Error("AI Model not found");
    }

    const questionTokens = getTokenLength(prompt);
    const answerTokens = ((ai.options as JsonObject)?.maxTokens ||
      aiModel.options.maxTokens.default) as number;

    const remainingTokens =
      aiModel.contextSize - answerTokens - questionTokens - BUFFER_TOKENS;

    const { knowledge } = await pineconeAdapter.getKnowledge(
      prompt,
      chat.messages,
      chat.ai.dataSources,
      remainingTokens
    );

    let promptWithKnowledge;
    if (knowledge.length === 0) {
      promptWithKnowledge = prompt;
    } else {
      promptWithKnowledge = `
      ${prompt}\n
      Answer questions using this knowledge:\n
      ${knowledge}
      `;
    }

    const assistant = new OpenAIAssistantRunnable({
      assistantId: ai.externalId,
    });

    const response = (await assistant.invoke({
      threadId: chat.externalId,
      content: promptWithKnowledge,
    })) as ThreadMessage[];

    let responseText = "";
    if (response.length > 0) {
      // Get content from the last message
      const threadMessage = response[0];
      for (const content of threadMessage.content) {
        if (content.type === "text") {
          responseText = content.text.value;

          await chatService.updateChat(
            chatId,
            userId,
            responseText,
            Role.system
          );

          break;
        }
      }
    }

    return responseText;
  }
}

const openAIAssistantModelAdapter = new OpenAIAssistantModelAdapter();
export default openAIAssistantModelAdapter;
