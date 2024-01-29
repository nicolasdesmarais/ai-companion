import { AIModel, AIModelProvider } from "@/src/domain/models/AIModel";
import { Replicate } from "@langchain/community/llms/replicate";
import { CallbackManager } from "@langchain/core/callbacks/manager";
import { BaseCompletionModel } from "./BaseCompletionModel";
import { ChatModel } from "./ChatModel";

export class ReplicateModel extends BaseCompletionModel implements ChatModel {
  public supports(model: AIModel): boolean {
    return model.provider === AIModelProvider.REPLICATE;
  }

  protected getChatModelInstance(
    model: AIModel,
    options: any,
    customHandlers: any
  ) {
    return new Replicate({
      model: `${model.additionalData.owner}/${model.externalModelId}:${model.additionalData.version}`,
      input: {
        ...options,
        top_p: options.topP,
        max_tokens: options.maxTokens,
      },
      apiKey: process.env.REPLICATE_API_TOKEN,
      callbackManager: CallbackManager.fromHandlers(customHandlers),
    });
  }
}
