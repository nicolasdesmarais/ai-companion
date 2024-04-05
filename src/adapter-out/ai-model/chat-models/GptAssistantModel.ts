import { AIModel } from "@/src/domain/models/AIModel";
import aiModelService from "@/src/domain/services/AIModelService";
import { OpenAIAssistantRunnable } from "langchain/experimental/openai_assistant";

import { AIRequest } from "@/src/adapter-in/api/AIApi";
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

  private getInstructions(ai: AIRequest) {
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
    const aiModel = await aiModelService.getAIModelById(ai.modelId);
    const assistantConfiguration = await this.getAssistantConfiguration(
      ai,
      aiModel
    );

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
    const { assistantId, ai } = input;
    const aiModel = await aiModelService.getAIModelById(ai.modelId);

    const assistantConfiguration = await this.getAssistantConfiguration(
      ai,
      aiModel
    );

    await OPEN_AI.beta.assistants.update(assistantId, assistantConfiguration);
  }

  private async getAssistantConfiguration(ai: AIRequest, aiModel: AIModel) {
    const instructions = this.getInstructions(ai);

    return {
      model: aiModel.externalModelId,
      name: ai.name,
      description: ai.description,
      instructions: instructions,
    };
  }

  public async deleteAssistant(
    aiModelId: string,
    externalId: string
  ): Promise<void> {
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

    let response = (await assistant.invoke({
      threadId: externalChatId,
      content: promptWithKnowledge,
    })) as ThreadMessage[];

    if (response.length === 0) {
      // thread expired?
      externalChatId = await this.createChat();
      response = (await assistant.invoke({
        threadId: externalChatId,
        content: promptWithKnowledge,
      })) as ThreadMessage[];
    }

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
