import { ChatAnthropicMessages } from "@langchain/anthropic";
import { BaseChatModel } from "@langchain/core/language_models/chat_models";
import { AbstractBaseChatModel } from "./AbstractBaseChatModel";
import { ChatModel } from "./ChatModel";

const MODEL_ID = "anthropic";

export class AnthropicModel extends AbstractBaseChatModel implements ChatModel {
  public supports(modelId: string): boolean {
    return modelId === MODEL_ID;
  }

  protected getChatModelInstance(options: any): BaseChatModel {
    return new ChatAnthropicMessages({
      anthropicApiKey: process.env.ANTHROPIC_API_KEY,
      ...options,
    });
  }
}
