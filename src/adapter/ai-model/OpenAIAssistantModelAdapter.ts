import { AIModel } from "@/src/domain/models/AIModel";
import { AI } from "@prisma/client";
import OpenAI from "openai";

const openai = new OpenAI({
  apiKey: process.env.OPENAI_API_KEY,
});

export class OpenAIAssistantModelAdapter {
  public async createExternalAI(ai: AI, aiModel: AIModel): Promise<string> {
    const assistant = await openai.beta.assistants.create({
      model: aiModel.externalModelId,
      name: ai.name,
      description: ai.description,
      instructions: ai.instructions,
      tools: [
        {
          type: "code_interpreter",
        },
        {
          type: "retrieval",
        },
      ],
    });

    return assistant.id;
  }
}

const openAIAssistantModelAdapter = new OpenAIAssistantModelAdapter();
export default openAIAssistantModelAdapter;
