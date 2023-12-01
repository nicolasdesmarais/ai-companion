import { OpenAI } from "langchain/llms/openai";
import { ChatModel, PostToChatInput } from "./ChatModel";

const MODEL_ID = "text-davinci-003";

export class DavinciModel implements ChatModel {
  supports(modelId: string): boolean {
    return modelId === MODEL_ID;
  }
  private getInstance(options: any, customHandlers: any): any {
    new OpenAI({
      openAIApiKey: process.env.OPENAI_API_KEY,
      modelName: MODEL_ID,
      ...options,
    });
  }

  public async postToChat(input: PostToChatInput): Promise<any> {
    throw new Error("Method not implemented.");
  }
}
