import { AIModel } from "@/src/domain/models/AIModel";
import { AI } from "@prisma/client";
import { OpenAIAssistantRunnable } from "langchain/experimental/openai_assistant";
import OpenAI from "openai";

const openai = new OpenAI({
  apiKey: process.env.OPENAI_API_KEY,
});

export class OpenAIAssistantModelAdapter {
  public async createExternalAI(ai: AI, aiModel: AIModel): Promise<string> {
    const lca = await OpenAIAssistantRunnable.createAssistant({
      model: "gpt-4-1106-preview",
    });

    const instructions = `
        Pretend you are ${ai.name}, ${ai.description}.
        Output format is markdown. Open links in new tabs.
        Here are more details about your character:\n
        ${ai.instructions}

        Below are relevant details about ${ai.name}'s past:\n
        ${ai.seed}
      `;

    const assistant = await openai.beta.assistants.create({
      model: aiModel.externalModelId,
      name: ai.name,
      description: ai.description,
      instructions: instructions,

      tools: [
        {
          type: "code_interpreter",
        },
      ],
    });

    return assistant.id;
  }

  public async createExternalChat() {
    const thread = await openai.beta.threads.create();
    return thread.id;
  }
}

const openAIAssistantModelAdapter = new OpenAIAssistantModelAdapter();
export default openAIAssistantModelAdapter;
