import { ChatCohere } from "@langchain/cohere";
import { BaseChatModel as BaseLangChainChatModel } from "@langchain/core/language_models/chat_models";
import { BaseChatModel } from "./BaseChatModel";
import { ChatModel } from "./ChatModel";

const MODEL_ID = "cohere";

export class CohereModel extends BaseChatModel implements ChatModel {
  public supports(modelId: string): boolean {
    return modelId === MODEL_ID;
  }

  protected getChatModelInstance(options: any): BaseLangChainChatModel {
    return new ChatCohere({
      apiKey: process.env.COHERE_API_KEY,
      model: "command",
      ...options,
    });
  }
}
