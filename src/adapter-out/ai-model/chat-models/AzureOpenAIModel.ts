import { AIModel, AIModelProvider } from "@/src/domain/models/AIModel";
import { BaseChatModel } from "@langchain/core/language_models/chat_models";
import { ChatOpenAI } from "@langchain/openai";
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
    return new ChatOpenAI({
      azureOpenAIApiKey: model.additionalData.apiKey,
      azureOpenAIApiVersion: model.additionalData.apiVersion,
      azureOpenAIApiInstanceName: model.additionalData.instanceName,
      azureOpenAIApiDeploymentName: model.additionalData.deploymentName,
      modelName: model.externalModelId,
      streaming: true,
      callbacks,
      ...options,
    });
  }
}
