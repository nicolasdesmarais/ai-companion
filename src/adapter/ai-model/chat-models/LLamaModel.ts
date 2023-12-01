import { CallbackManager } from "langchain/callbacks";
import { Replicate } from "langchain/llms/replicate";
import { ChatModel, PostToChatInput } from "./ChatModel";

const MODEL_ID = "llama2-13b";
const REPLICATE_MODEL =
  "meta/llama-2-13b-chat:f4e2de70d66816a838a89eeeb621910adffb0dd0baba3976c96980970978018d";

export class LLamaModel implements ChatModel {
  public supports(modelId: string): boolean {
    return modelId === MODEL_ID;
  }

  private getInstance(options: any, customHandlers: any) {
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

  public async postToChat(input: PostToChatInput) {
    throw new Error("Method not implemented.");
  }
}
