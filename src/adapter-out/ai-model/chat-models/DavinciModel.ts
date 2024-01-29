import { AIModel } from "@/src/domain/models/AIModel";
import { OpenAI } from "@langchain/openai";
import { BaseCompletionModel } from "./BaseCompletionModel";
import { ChatModel } from "./ChatModel";

const MODEL_ID = "text-davinci-003";

export class DavinciModel extends BaseCompletionModel implements ChatModel {
  supports(model: AIModel): boolean {
    return model.id === MODEL_ID;
  }
  protected getChatModelInstance(
    model: AIModel,
    options: any,
    customHandlers: any
  ): any {
    return new OpenAI({
      openAIApiKey: process.env.OPENAI_API_KEY,
      modelName: MODEL_ID,
      ...options,
    });
  }
}
