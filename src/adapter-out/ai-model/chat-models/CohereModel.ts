import { AIModel } from "@/src/domain/models/AIModel";
import { ChatCohere } from "@langchain/cohere";
import { BaseChatModel as BaseLangChainChatModel } from "@langchain/core/language_models/chat_models";
import { AbstractBaseChatModel } from "./AbstractBaseChatModel";
import { ChatModel } from "./ChatModel";

const MODEL_ID = "cohere";

export class CohereModel extends AbstractBaseChatModel implements ChatModel {
  public supports(model: AIModel): boolean {
    return model.id === MODEL_ID;
  }

  protected getChatModelInstance(
    options: any,
    callbackHandler: any
  ): BaseLangChainChatModel {
    return new ChatCohere({
      apiKey: process.env.COHERE_API_KEY,
      model: "command",
      callbacks: [callbackHandler],
      ...options,
    });
  }
}
