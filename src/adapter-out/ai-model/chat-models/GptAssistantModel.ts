import { AIModel } from "@/src/domain/models/AIModel";
import aiModelService from "@/src/domain/services/AIModelService";
import { AssistantsClient, AzureKeyCredential } from "@azure/openai-assistants";
import { AI } from "@prisma/client";

import {
  AssistantChatModel,
  CreateAssistantInput,
  UpdateAssistantInput,
} from "./AssistantChatModel";
import { ChatModel, PostToChatInput, PostToChatResponse } from "./ChatModel";

const MODEL_ID = "gpt-4-1106-preview-assistant";
const AZURE_ENDPOINT = "https://prod-appdirectai-east2.openai.azure.com/";
const AZURE_KEY = process.env.AZURE_GPT40_KEY!;

export class GptAssistantModel implements ChatModel, AssistantChatModel {
  public supports(model: AIModel): boolean {
    return model.id === MODEL_ID;
  }

  private getAssistantsClient() {
    return new AssistantsClient(
      AZURE_ENDPOINT,
      new AzureKeyCredential(AZURE_KEY)
    );
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

    const assistantsClient = this.getAssistantsClient();
    const assistant = await assistantsClient.createAssistant({
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

    const assistantsClient = this.getAssistantsClient();

    await assistantsClient.updateAssistant(
      ai.externalId,
      assistantConfiguration
    );
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
    const assistantsClient = this.getAssistantsClient();
    await assistantsClient.deleteAssistant(externalId);
  }

  private async createChat(): Promise<string> {
    const assistantsClient = this.getAssistantsClient();
    const thread = await assistantsClient.createThread();
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

    let externalChatId = chat.externalId;
    if (!externalChatId) {
      externalChatId = await this.createChat();
    }

    const assistantsClient = this.getAssistantsClient();
    await assistantsClient.createMessage(
      externalChatId,
      "user",
      promptWithKnowledge
    );

    let runResponse = await assistantsClient.createRun(externalChatId, {
      assistantId: ai.externalId,
    });

    do {
      await new Promise((resolve) => setTimeout(resolve, 800));
      runResponse = await assistantsClient.getRun(
        externalChatId,
        runResponse.id
      );
    } while (
      runResponse.status === "queued" ||
      runResponse.status === "in_progress"
    );

    const runMessages = await assistantsClient.listMessages(externalChatId);
    let responseText = "";
    for (const runMessageDatum of runMessages.data) {
      for (const item of runMessageDatum.content) {
        if (item.type === "text") {
          responseText += item.text.value + "\n";
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
