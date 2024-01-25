import { getTokenLength } from "@/src/lib/tokenCount";
import { AI, Message, Role } from "@prisma/client";
import { Cohere, CohereClient } from "cohere-ai";
import { ChatModel, PostToChatInput, PostToChatResponse } from "./ChatModel";

const MODEL_ID = "cohere";
const COHERE_API_KEY = process.env.COHERE_API_KEY ?? "";

const client = new CohereClient({
  token: COHERE_API_KEY,
});

export class CohereModel implements ChatModel {
  public supports(modelId: string): boolean {
    return modelId === MODEL_ID;
  }

  public async postToChat(input: PostToChatInput): Promise<PostToChatResponse> {
    const {
      ai,
      date,
      messages,
      chat,
      prompt,
      getKnowledgeCallback,
      endCallback,
    } = input;

    const chatHistory = this.getChatHistory(ai, messages);
    const engineeredPrompt = `
        Pretend you are ${ai.name}, ${ai.description}.
        The user date and time is ${date}. Output format is markdown, including tables.
        Here are more details about your character:\n
        ${ai.instructions}
        Answer questions using this knowledge:\n
      `;
    const instructionTokens = getTokenLength(engineeredPrompt);
    const chatHistoryTokens = getTokenLength(JSON.stringify(chatHistory));
    const tokensUsed = instructionTokens + chatHistoryTokens;

    const knowledge = await getKnowledgeCallback(tokensUsed);

    const engineeredPromptMessage: Cohere.ChatMessage = {
      role: Cohere.ChatMessageRole.Chatbot,
      message: engineeredPrompt,
    };

    chatHistory.unshift(engineeredPromptMessage);

    const chatStream = await client.chatStream({
      chatHistory,
      message: prompt,
      connectors: [{ id: "web-search" }],
    });

    return {
      isStream: true,
      response: chatStream,
    };
  }

  private getChatHistory(ai: AI, messages: Message[]): Cohere.ChatMessage[] {
    const chatHistoryWithSeed: Cohere.ChatMessage[] = [];
    chatHistoryWithSeed.push(...this.getHistorySeed(ai));

    const chatHistory = messages.map((message) => {
      const role =
        message.role === Role.user
          ? Cohere.ChatMessageRole.User
          : Cohere.ChatMessageRole.Chatbot;
      return {
        role,
        message: message.content,
      };
    });

    chatHistoryWithSeed.push(...chatHistory);
    return chatHistoryWithSeed;
  }

  private getHistorySeed(ai: AI): Cohere.ChatMessage[] {
    let historySeed: Cohere.ChatMessage[] = [];
    if (ai.seed) {
      historySeed = ai.seed
        .split("\n\n")
        .reduce((result: any, line: string) => {
          if (line.trimStart().startsWith(ai.name)) {
            result.push({
              role: Cohere.ChatMessageRole.Chatbot,
              message: line.replace(ai.name + ":", "").trimStart(),
            });
          } else {
            result.push({
              role: Cohere.ChatMessageRole.User,
              message: line.trimStart().replace("Human:", "").trimStart(),
            });
          }
          return result;
        }, []);
    }

    return historySeed;
  }
}
