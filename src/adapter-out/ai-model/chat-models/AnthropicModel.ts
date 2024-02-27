import { AIModel } from "@/src/domain/models/AIModel";
import { ChatAnthropicMessages } from "@langchain/anthropic";
import { BaseChatModel } from "@langchain/core/language_models/chat_models";
import { AbstractBaseChatModel } from "./AbstractBaseChatModel";
import { ChatModel } from "./ChatModel";

const MODEL_ID = "anthropic";

export class AnthropicModel extends AbstractBaseChatModel implements ChatModel {
  public supports(model: AIModel): boolean {
    return model.id === MODEL_ID;
  }

  protected getChatModelInstance(
    options: any,
    callbackHandler: any
  ): BaseChatModel {
    return new ChatAnthropicMessages({
      anthropicApiKey: process.env.ANTHROPIC_API_KEY,
      callbacks: [callbackHandler],
      ...options,
    });
  }
}
