import { AIModel, AIModelProvider } from "@/src/domain/models/AIModel";
import { ChatAnthropicMessages } from "@langchain/anthropic";
import { BaseChatModel } from "@langchain/core/language_models/chat_models";
import { AbstractBaseChatModel } from "./AbstractBaseChatModel";
import { ChatModel } from "./ChatModel";

export class AnthropicModel extends AbstractBaseChatModel implements ChatModel {
  public supports(model: AIModel): boolean {
    return model.provider === AIModelProvider.ANTHROPIC;
  }

  protected getChatModelInstance(
    model: AIModel,
    options: any,
    callbacks: any
  ): BaseChatModel {
    return new ChatAnthropicMessages({
      anthropicApiKey: process.env.ANTHROPIC_API_KEY,
      modelName: model.externalModelId,
      callbacks,
      ...options,
    });
  }
}
