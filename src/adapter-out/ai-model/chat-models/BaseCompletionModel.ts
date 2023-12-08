import { getTokenLength } from "@/src/lib/tokenCount";
import { Message } from "@prisma/client";
import { LangChainStream } from "ai";
import { PostToChatInput, PostToChatResponse } from "./ChatModel";

export abstract class BaseCompletionModel {
  protected abstract getChatModelInstance(
    options: any,
    customHandlers: any
  ): any;

  public async postToChat(input: PostToChatInput): Promise<PostToChatResponse> {
    const { ai, messages, date, options, getKnowledgeCallback, endCallback } =
      input;

    const { handlers } = LangChainStream();
    const customHandleLLMEnd = async (_output: any, runId: string) => {
      await endCallback(_output.generations[0][0].text);
      return await handlers.handleLLMEnd(_output, runId);
    };

    const customHandlers = {
      ...handlers,
      handleLLMEnd: customHandleLLMEnd,
    };

    const completionModel = this.getChatModelInstance(options, customHandlers);

    const chatHistory = messages.reduce((acc: string, message: Message) => {
      if (message.role === "user") {
        return acc + `User: ${message.content}\n`;
      } else {
        return acc + `${ai.name}: ${message.content}\n`;
      }
    }, "");
    const completionPrompt = `
        ONLY generate plain sentences without prefix of who is speaking. DO NOT use ${ai.name}: prefix.
        The user date and time is ${date}. Output format is markdown, including tables.
        ${ai.instructions}
        Below are relevant details about ${ai.name}'s past and the conversation you are in:\n
      `;
    const instructionTokens = getTokenLength(completionPrompt);
    const seededChatHistory = `${ai.seed}\n\n${chatHistory}\n${ai.name}:`;
    const chatHistoryTokens = getTokenLength(seededChatHistory);
    const tokensUsed = instructionTokens + chatHistoryTokens;

    const knowledge = await getKnowledgeCallback(tokensUsed);

    let response = await completionModel.call(
      `${completionPrompt}${knowledge}${seededChatHistory}`
    );
    response = response.replaceAll(",", "");
    const Readable = require("stream").Readable;
    let s = new Readable();
    s.push(response);
    s.push(null);
    return {
      isStream: true,
      response: s,
    };
  }
}
