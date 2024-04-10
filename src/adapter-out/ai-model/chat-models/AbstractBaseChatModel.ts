import { AIModel } from "@/src/domain/models/AIModel";
import { ChatAiForWriteDto, ChatMessageDto } from "@/src/domain/models/Chats";
import { getTokenLength } from "@/src/lib/tokenCount";
import {
  AIMessage,
  HumanMessage,
  SystemMessage,
} from "@langchain/core/messages";
import { Runnable } from "@langchain/core/runnables";
import { HttpResponseOutputParser } from "langchain/output_parsers";
import { PostToChatInput, PostToChatResponse } from "./ChatModel";

export abstract class AbstractBaseChatModel {
  protected abstract getChatModelInstance(
    model: AIModel,
    options: any,
    callbacks: any
  ): Runnable;

  public async postToChat(input: PostToChatInput): Promise<PostToChatResponse> {
    const stream = await this.createStream(input);

    return {
      isStream: true,
      response: stream,
    };
  }

  protected async createStream(
    input: PostToChatInput
  ): Promise<ReadableStream> {
    const { chat, aiModel, messages, date, getKnowledgeCallback } = input;
    const { ai } = chat;

    const engineeredPrompt = this.createEngineeredPrompt(ai, date);

    const historySeed = this.createHistorySeed(ai);

    const historyMessages = this.createHistoryMessages(messages);

    const tokensUsed = this.calculateTokensUsed([
      engineeredPrompt,
      historyMessages,
      historySeed,
    ]);

    const knowledge = await getKnowledgeCallback(input, tokensUsed);

    const chatLog = this.ensureAlternatingMessages([
      new SystemMessage(`${engineeredPrompt}${knowledge.knowledge}\n`),
      ...historySeed,
      ...historyMessages,
    ]);

    const callbacks = this.getCallbacks(input);
    const chatModel = this.getChatModelInstance(
      aiModel,
      input.options,
      callbacks
    );

    const parser = new HttpResponseOutputParser();
    return await chatModel.pipe(parser).stream(chatLog);
  }

  protected createEngineeredPrompt(
    ai: ChatAiForWriteDto,
    date: string
  ): string {
    return `
      Pretend you are ${ai.name}, ${ai.description}.
      The user date and time is ${date}. Output format is markdown, including tables.
      DO NOT use ${ai.name}: prefix.
      Here are more details about your character:\n
      ${ai.instructions}
      Answer questions using this knowledge:\n
    `;
  }

  protected createHistorySeed(
    ai: ChatAiForWriteDto
  ): (HumanMessage | AIMessage)[] {
    return this.parseSeed(ai);
  }

  private parseSeed(ai: ChatAiForWriteDto): (HumanMessage | AIMessage)[] {
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

  protected createHistoryMessages(
    messages: ChatMessageDto[]
  ): (HumanMessage | AIMessage)[] {
    return this.parseMessages(messages);
  }

  protected parseMessages(
    messages: ChatMessageDto[]
  ): (HumanMessage | AIMessage)[] {
    return messages.map((message) =>
      message.role === "user"
        ? new HumanMessage(message.content)
        : new AIMessage(message.content)
    );
  }

  protected getCallbacks(input: PostToChatInput): any {
    const { callbackContext, startChatCallback, endChatCallback } = input;

    return [
      {
        handleLLMEnd: async (_output: any, runId: string) => {
          await endChatCallback(
            callbackContext,
            _output.generations[0][0].text
          );
        },
        handleLLMNewToken: async (token: string) => {
          startChatCallback(callbackContext);
        },
      },
    ];
  }

  protected calculateTokensUsed(
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
