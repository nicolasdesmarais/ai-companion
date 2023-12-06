import { OpenAI } from "langchain/llms/openai";
import { BaseCompletionModel } from "./BaseCompletionModel";
import { ChatModel } from "./ChatModel";

const MODEL_ID = "text-davinci-003";

export class DavinciModel extends BaseCompletionModel implements ChatModel {
  supports(modelId: string): boolean {
    return modelId === MODEL_ID;
  }
  protected getChatModelInstance(options: any, customHandlers: any): any {
    return new OpenAI({
      openAIApiKey: process.env.OPENAI_API_KEY,
      modelName: MODEL_ID,
      ...options,
    });
  }
}
