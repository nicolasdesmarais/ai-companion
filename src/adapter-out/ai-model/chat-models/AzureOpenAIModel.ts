import { AIModel, AIModelProvider } from "@/src/domain/models/AIModel";
import { AzureChatOpenAI } from "@langchain/azure-openai";
import { BaseChatModel } from "@langchain/core/language_models/chat_models";
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
      azureOpenAIEndpoint: `https://${model.additionalData.instanceName}.openai.azure.com`,
      azureOpenAIApiKey: model.additionalData.apiKey,
      azureOpenAIApiVersion: model.additionalData.apiVersion,
      azureOpenAIApiDeploymentName: model.additionalData.deploymentName,
      modelName: model.externalModelId,
      streaming: true,
      callbacks,
      ...options,
    });
  }
}
