import { AIModel } from "@/src/domain/models/AIModel";
import { CallbackManager } from "langchain/callbacks";
import { Replicate } from "langchain/llms/replicate";
import { BaseCompletionModel } from "./BaseCompletionModel";
import { ChatModel } from "./ChatModel";

const MODEL_ID = "llama2-13b";
const REPLICATE_MODEL =
  "meta/llama-2-13b-chat:f4e2de70d66816a838a89eeeb621910adffb0dd0baba3976c96980970978018d";

export class LLamaModel extends BaseCompletionModel implements ChatModel {
  public supports(model: AIModel): boolean {
    return model.id === MODEL_ID;
  }

  protected getChatModelInstance(options: any, customHandlers: any) {
    return new Replicate({
      model: REPLICATE_MODEL,
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
