import { AIModel, AIModelProvider } from "@/src/domain/models/AIModel";
import { BaseChatModel } from "@langchain/core/language_models/chat_models";
import { AzureChatOpenAI } from "@langchain/openai";
import { AbstractBaseChatModel } from "./AbstractBaseChatModel";
import { ChatModel } from "./ChatModel";

export class AzureOpenAIModel
  extends AbstractBaseChatModel
  implements ChatModel
{
  public supports(model: AIModel): boolean {
    return model.provider === AIModelProvider.AZURE_OPENAI;
  }

  protected getChatModelInstance(
    model: AIModel,
    options: any,
    callbacks: any
  ): BaseChatModel {
    return new AzureChatOpenAI({
      openAIApiKey: model.additionalData.apiKey,
      openAIApiVersion: model.additionalData.apiVersion,
      deploymentName: model.additionalData.deploymentName,
      modelName: model.externalModelId,
      streaming: true,
      callbacks,
      ...options,
    });
  }
}
