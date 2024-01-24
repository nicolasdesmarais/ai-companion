import {
  ChatOpenAI,
  ChatOpenAICallOptions,
} from "langchain/chat_models/openai";
import { AbstractBaseChatModel } from "./AbstractBaseChatModel";
import { ChatModel } from "./ChatModel";

const MODEL_ID = "gpt35-16k";

//TODO: Move these to environment variables
const AZURE_OPENAI_API_VERSION = "2023-05-15";
const AZURE_OPENAI_API_INSTANCE_NAME = "appdirect-prod-ai-useast";
const AZURE_OPENAI_API_DEPLOYMENT_NAME = "ai-prod-16k";

export class Gpt35Model extends AbstractBaseChatModel implements ChatModel {
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
