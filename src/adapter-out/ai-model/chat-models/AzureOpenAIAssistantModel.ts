import { AIModel, AIModelProvider } from "@/src/domain/models/AIModel";
import aiModelService from "@/src/domain/services/AIModelService";
import { AssistantsClient, AzureKeyCredential } from "@azure/openai-assistants";

import { AIRequest } from "@/src/adapter-in/api/AIApi";
import {
  AssistantChatModel,
  CreateAssistantInput,
  UpdateAssistantInput,
} from "./AssistantChatModel";
import { ChatModel, PostToChatInput, PostToChatResponse } from "./ChatModel";

export class AzureOpenAIAssistantModel
  implements ChatModel, AssistantChatModel
{
  public supports(model: AIModel): boolean {
    return model.provider === AIModelProvider.AZURE_OPENAI_ASSISTANTS;
  }

  private getAssistantsClient(aiModel: AIModel) {
    const endpoint = `https://${aiModel.additionalData.instanceName}.openai.azure.com`;
    return new AssistantsClient(
      endpoint,
      new AzureKeyCredential(aiModel.additionalData.apiKey)
    );
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

    const assistantsClient = this.getAssistantsClient(aiModel);
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
    const { assistantId, ai } = input;
    const aiModel = await aiModelService.getAIModelById(ai.modelId);

    const assistantConfiguration = await this.getAssistantConfiguration(
      ai,
      aiModel
    );

    const assistantsClient = this.getAssistantsClient(aiModel);

    await assistantsClient.updateAssistant(assistantId, assistantConfiguration);
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
    const aiModel = await aiModelService.getAIModelById(aiModelId);
    const assistantsClient = this.getAssistantsClient(aiModel);
    await assistantsClient.deleteAssistant(externalId);
  }

  private async createChat(aiModel: AIModel): Promise<string> {
    const assistantsClient = this.getAssistantsClient(aiModel);
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

    const aiModel = await aiModelService.getAIModelById(ai.modelId);
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
      externalChatId = await this.createChat(aiModel);
    }

    const assistantsClient = this.getAssistantsClient(aiModel);
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
