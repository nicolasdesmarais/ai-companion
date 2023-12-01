import { OpenAIAssistantRunnable } from "langchain/experimental/openai_assistant";
import { ThreadMessage } from "openai/resources/beta/threads/messages/messages.mjs";
import { ChatModel, PostToChatInput, PostToChatResponse } from "./ChatModel";

const MODEL_ID = "gpt-4-1106-preview-assistant";

export class GptAssistantModel implements ChatModel {
  public supports(modelId: string): boolean {
    return modelId === MODEL_ID;
  }

  public async postToChat(input: PostToChatInput): Promise<PostToChatResponse> {
    const { ai, chat, prompt, getKnowledgeCallback, endCallback } = input;

    if (!ai.externalId) {
      throw new Error("AI does not have an external ID");
    }

    const { knowledge } = await getKnowledgeCallback(0);

    let promptWithKnowledge;
    if (knowledge.length === 0) {
      promptWithKnowledge = prompt;
    } else {
      promptWithKnowledge = `
      ${prompt}\n
      Answer questions using this knowledge:\n
      ${knowledge}
      `;
    }

    const assistant = new OpenAIAssistantRunnable({
      assistantId: ai.externalId,
    });

    const response = (await assistant.invoke({
      threadId: chat.externalId,
      content: promptWithKnowledge,
    })) as ThreadMessage[];

    let responseText = "";
    if (response.length > 0) {
      // Get content from the last message
      const threadMessage = response[0];
      for (const content of threadMessage.content) {
        if (content.type === "text") {
          responseText = content.text.value;
          break;
        }
      }
    }

    await endCallback(responseText);

    return {
      isStream: false,
      response: responseText,
    };
  }
}
