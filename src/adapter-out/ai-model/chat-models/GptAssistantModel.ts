import { AIModel } from "@/src/domain/models/AIModel";
import aiModelService from "@/src/domain/services/AIModelService";
import { AI } from "@prisma/client";
import { OpenAIAssistantRunnable } from "langchain/experimental/openai_assistant";

import OpenAI from "openai";
import { ThreadMessage } from "openai/resources/beta/threads/messages/messages.mjs";
import {
  AssistantChatModel,
  CreateAssistantInput,
  UpdateAssistantInput,
} from "./AssistantChatModel";
import { ChatModel, PostToChatInput, PostToChatResponse } from "./ChatModel";

const MODEL_ID = "gpt-4-1106-preview-assistant";

const OPEN_AI = new OpenAI({
  apiKey: process.env.OPENAI_API_KEY,
});

export class GptAssistantModel implements ChatModel, AssistantChatModel {
  public supports(model: AIModel): boolean {
    return model.id === MODEL_ID;
  }

  private getInstructions(ai: AI) {
    return `
        Pretend you are ${ai.name}, ${ai.description}.
        Output format is markdown, including tables.
        DO NOT use ${ai.name}: prefix.
        Here are more details about your character:\n
        ${ai.instructions}

        Below are relevant details about ${ai.name}'s past:\n
        ${ai.seed}
      `;
  }

  public async createAssistant(input: CreateAssistantInput): Promise<string> {
    const { ai } = input;
    const assistantConfiguration = await this.getAssistantConfiguration(ai);

    const assistant = await OPEN_AI.beta.assistants.create({
      ...assistantConfiguration,
      tools: [
        {
          type: "code_interpreter",
        },
      ],
    });

    return assistant.id;
  }

  public async updateAssistant(input: UpdateAssistantInput): Promise<void> {
    const { ai } = input;
    if (!ai.externalId) {
      throw new Error("Missing AI external ID");
    }

    const assistantConfiguration = await this.getAssistantConfiguration(ai);

    await OPEN_AI.beta.assistants.update(ai.externalId, assistantConfiguration);
  }

  private async getAssistantConfiguration(ai: AI) {
    const aiModel = await aiModelService.findAIModelById(ai.modelId);
    if (!aiModel) {
      throw new Error("AI model not found");
    }

    const instructions = this.getInstructions(ai);

    return {
      model: aiModel.externalModelId,
      name: ai.name,
      description: ai.description,
      instructions: instructions,
    };
  }

  public async deleteAssistant(externalId: string): Promise<void> {
    await OPEN_AI.beta.assistants.del(externalId);
  }

  private async createChat(): Promise<string> {
    const thread = await OPEN_AI.beta.threads.create();
    return thread.id;
  }

  public async postToChat(input: PostToChatInput): Promise<PostToChatResponse> {
    const {
      chat,
      prompt,
      callbackContext,
      getKnowledgeCallback,
      startChatCallback,
      endChatCallback,
    } = input;
    const { ai } = chat;

    if (!ai.externalId) {
      throw new Error("AI does not have an external ID");
    }

    const { knowledge } = await getKnowledgeCallback(input, 0);

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

    let externalChatId = chat.externalId;
    if (!externalChatId) {
      externalChatId = await this.createChat();
    }

    const response = (await assistant.invoke({
      threadId: externalChatId,
      content: promptWithKnowledge,
    })) as ThreadMessage[];

    let responseText = "";
    if (response.length > 0) {
      // Get content from the last message
      const threadMessage = response[0];
      for (const content of threadMessage.content) {
        if (content.type === "text") {
          responseText = content.text.value;
          break;
        }
      }
    }

    await startChatCallback(callbackContext);
    await endChatCallback(callbackContext, responseText, externalChatId);

    return {
      isStream: false,
      response: responseText,
    };
  }
}
