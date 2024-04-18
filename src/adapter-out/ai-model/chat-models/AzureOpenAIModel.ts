import { AIModel, AIModelProvider } from "@/src/domain/models/AIModel";
import { BaseChatModel } from "@langchain/core/language_models/chat_models";
import { ChatOpenAI } from "@langchain/openai";
import { AbstractBaseChatModel } from "./AbstractBaseChatModel";
import { ChatModel } from "./ChatModel";

const AZURE_OPENAI_API_VERSION = "2024-02-01";
const AZURE_OPENAI_API_INSTANCE_NAME = "prod-appdirectai-east2";
const AZURE_OPENAI_API_DEPLOYMENT_NAME = "gpt4-32k";

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
      streaming: true,
      callbacks,
      ...options,
    });
  }
}

export const gpt4ChatModel = new ChatOpenAI({
  azureOpenAIApiKey: process.env.AZURE_GPT40_KEY,
  azureOpenAIApiVersion: AZURE_OPENAI_API_VERSION,
  azureOpenAIApiInstanceName: AZURE_OPENAI_API_INSTANCE_NAME,
  azureOpenAIApiDeploymentName: AZURE_OPENAI_API_DEPLOYMENT_NAME,
});
