import { AIModel } from "@/src/domain/models/AIModel";
import { ChatCohere } from "@langchain/cohere";
import { BaseChatModel as BaseLangChainChatModel } from "@langchain/core/language_models/chat_models";
import { HumanMessage } from "@langchain/core/messages";
import { AbstractBaseChatModel } from "./AbstractBaseChatModel";
import { ChatModel } from "./ChatModel";

const MODEL_ID = "cohere";

export class CohereModel extends AbstractBaseChatModel implements ChatModel {
  public supports(model: AIModel): boolean {
    return model.id === MODEL_ID;
  }

  protected getChatModelInstance(
    model: AIModel,
    options: any,
    callbacks: any
  ): BaseLangChainChatModel {
    return new ChatCohere({
      apiKey: process.env.COHERE_API_KEY,
      model: "command",
      callbacks,
      ...options,
    });
  }

  protected createEngineeredPromptMessage(
    engineeredPrompt: string,
    knowledge: string
  ) {
    return new HumanMessage(`${engineeredPrompt}${knowledge}\n`);
  }
}
