import { getTokenLength } from "@/src/lib/tokenCount";
import { BaseChatModel } from "@langchain/core/language_models/chat_models";
import {
  AIMessage,
  HumanMessage,
  SystemMessage,
} from "@langchain/core/messages";
import { AI, Message } from "@prisma/client";
import { HttpResponseOutputParser } from "langchain/output_parsers";
import { PostToChatInput, PostToChatResponse } from "./ChatModel";

export abstract class AbstractBaseChatModel {
  protected abstract getChatModelInstance(
    options: any,
    callbackHandler: any
  ): BaseChatModel;

  public async postToChat(input: PostToChatInput): Promise<PostToChatResponse> {
    const { ai, messages, date, getKnowledgeCallback, endCallback } = input;

    const callbacks = [
      {
        handleLLMEnd: async (_output: any, runId: string) => {
          await endCallback(_output.generations[0][0].text);
        },
      },
    ];

    const chatModel = this.getChatModelInstance(input.options, callbacks);

    const historySeed = this.ensureAlternatingMessages(this.parseSeed(ai));

    const historyMessages = this.ensureAlternatingMessages(
      this.parseMessages(messages)
    );

    const engineeredPrompt = this.createEngineeredPrompt(ai, date);

    const tokensUsed = this.calculateTokensUsed([
      engineeredPrompt,
      historyMessages,
      historySeed,
    ]);

    const knowledge = await getKnowledgeCallback(tokensUsed);

    const chatLog = [
      new SystemMessage(`${engineeredPrompt}${knowledge.knowledge}\n`),
      ...historySeed,
      ...historyMessages,
    ];

    const parser = new HttpResponseOutputParser();
    const stream = await chatModel.pipe(parser).stream(chatLog);

    return {
      isStream: true,
      response: stream,
    };
  }

  private parseSeed(ai: AI): (HumanMessage | AIMessage)[] {
    if (!ai.seed) {
      return [];
    }
    return ai.seed.split("\n\n").map((line) => this.parseLine(line, ai.name));
  }

  private parseLine(line: string, aiName: string): HumanMessage | AIMessage {
    const isAIMessage = line.trimStart().startsWith(aiName);
    const content = line
      .trimStart()
      .replace(`${isAIMessage ? aiName + ":" : "Human:"}`, "")
      .trimStart();
    return isAIMessage ? new AIMessage(content) : new HumanMessage(content);
  }

  private parseMessages(messages: Message[]): (HumanMessage | AIMessage)[] {
    return messages.map((message) =>
      message.role === "user"
        ? new HumanMessage(message.content)
        : new AIMessage(message.content)
    );
  }

  private createEngineeredPrompt(ai: AI, date: string): string {
    return `
      Pretend you are ${ai.name}, ${ai.description}.
      The user date and time is ${date}. Output format is markdown, including tables.
      DO NOT use ${ai.name}: prefix.
      Here are more details about your character:\n
      ${ai.instructions}
      Answer questions using this knowledge:\n
    `;
  }

  private calculateTokensUsed(
    elements: (string | (HumanMessage | AIMessage)[])[]
  ): number {
    return elements.reduce(
      (total, element) =>
        total +
        getTokenLength(
          typeof element === "string" ? element : JSON.stringify(element)
        ),
      0
    );
  }

  private ensureAlternatingMessages(
    messages: (HumanMessage | AIMessage)[]
  ): (HumanMessage | AIMessage)[] {
    const result: (HumanMessage | AIMessage)[] = [];

    messages.forEach((message, index) => {
      if (index === 0) {
        result.push(message);
        return;
      }

      const prevMessage = result[result.length - 1];
      const isPrevHuman = prevMessage instanceof HumanMessage;
      const isCurrentHuman = message instanceof HumanMessage;

      if (isPrevHuman === isCurrentHuman) {
        // Insert an empty message of the opposite type if consecutive messages are of the same type
        const emptyMessage = isPrevHuman
          ? new AIMessage(".")
          : new HumanMessage(".");
        result.push(emptyMessage);
      }

      result.push(message);
    });

    return result;
  }
}
