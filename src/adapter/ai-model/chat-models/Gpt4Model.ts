import {
  ChatOpenAI,
  ChatOpenAICallOptions,
} from "langchain/chat_models/openai";
import { BaseChatModel } from "./BaseChatModel";
import { ChatModel } from "./ChatModel";

const MODEL_ID = "gpt-4";

//TODO: Move these to environment variables
const AZURE_OPENAI_API_VERSION = "2023-05-15";
const AZURE_OPENAI_API_INSTANCE_NAME = "prod-appdirectai-east2";
const AZURE_OPENAI_API_DEPLOYMENT_NAME = "gpt4-32k";

export class Gpt4Model extends BaseChatModel implements ChatModel {
  public supports(modelId: string): boolean {
    return modelId === MODEL_ID;
  }

  protected getChatModelInstance(
    options: any
  ): ChatOpenAI<ChatOpenAICallOptions> {
    return new ChatOpenAI({
      azureOpenAIApiKey: process.env.AZURE_GPT35_KEY,
      azureOpenAIApiVersion: AZURE_OPENAI_API_VERSION,
      azureOpenAIApiInstanceName: AZURE_OPENAI_API_INSTANCE_NAME,
      azureOpenAIApiDeploymentName: AZURE_OPENAI_API_DEPLOYMENT_NAME,
      streaming: true,
      ...options,
    });
  }
}
