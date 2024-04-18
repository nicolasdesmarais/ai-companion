import { AIModel, AIModelProvider } from "@/src/domain/models/AIModel";
import { Replicate } from "@langchain/community/llms/replicate";
import { AbstractBaseChatModel } from "./AbstractBaseChatModel";
import { ChatModel } from "./ChatModel";

export class ReplicateModel extends AbstractBaseChatModel implements ChatModel {
  public supports(model: AIModel): boolean {
    return model.provider === AIModelProvider.REPLICATE;
  }

  protected getChatModelInstance(model: AIModel, options: any, callbacks: any) {
    return new Replicate({
      model: `${model.additionalData.owner}/${model.externalModelId}:${model.additionalData.version}`,
      input: {
        ...options,
        top_p: options.topP,
        max_tokens: options.maxTokens,
      },
      apiKey: process.env.REPLICATE_API_TOKEN,
      callbacks,
    });
  }
}
