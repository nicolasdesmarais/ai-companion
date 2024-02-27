import { getTokenLength } from "@/src/lib/tokenCount";
import { BaseChatModel } from "@langchain/core/language_models/chat_models";
import { HumanMessage, SystemMessage } from "@langchain/core/messages";
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

    let historySeed = [];
    if (ai.seed) {
      historySeed = ai.seed
        .split("\n\n")
        .reduce((result: any, line: string) => {
          if (line.trimStart().startsWith(ai.name)) {
            result.push(
              new SystemMessage(line.replace(ai.name + ":", "").trimStart())
            );
          } else {
            result.push(
              new HumanMessage(
                line.trimStart().replace("Human:", "").trimStart()
              )
            );
          }
          return result;
        }, []);
    }
    const historyMessages = messages.map((message) => {
      if (message.role === "user") {
        return new HumanMessage(message.content);
      } else {
        return new SystemMessage(message.content);
      }
    });
    const engineeredPrompt = `
        Pretend you are ${ai.name}, ${ai.description}.
        The user date and time is ${date}. Output format is markdown, including tables.
        DO NOT use ${ai.name}: prefix.
        Here are more details about your character:\n
        ${ai.instructions}
        Answer questions using this knowledge:\n
      `;
    const instructionTokens = getTokenLength(engineeredPrompt);
    const chatHistoryTokens = getTokenLength(JSON.stringify(historyMessages));
    const chatSeedTokens = getTokenLength(JSON.stringify(historySeed));
    const tokensUsed = instructionTokens + chatHistoryTokens + chatSeedTokens;

    const knowledge = await getKnowledgeCallback(tokensUsed);
    const chatLog = [
      new SystemMessage(`${engineeredPrompt}${knowledge.knowledge}\n`),
    ];
    chatLog.push(...historySeed);
    chatLog.push(...historyMessages);

    const parser = new HttpResponseOutputParser();
    const stream = await chatModel.pipe(parser).stream(chatLog);

    return {
      isStream: true,
      response: stream,
    };
  }
}
