import { getTokenLength } from "@/src/lib/tokenCount";
import { LangChainStream } from "ai";
import axios from "axios";
import {
  ChatOpenAI,
  ChatOpenAICallOptions,
} from "langchain/chat_models/openai";
import { HumanMessage, SystemMessage } from "langchain/schema";
import pineconeAdapter from "../../knowledge/pinecone/PineconeAdapter";
import { PostToChatInput } from "./ChatModel";

const BUFFER_TOKENS = 200;

export abstract class BaseChatModel {
  protected abstract getChatModelInstance(
    options: any
  ): ChatOpenAI<ChatOpenAICallOptions>;

  public async postToChat(input: PostToChatInput) {
    const {
      ai,
      chat,
      messages,
      aiModel,
      prompt,
      date,
      answerTokens,
      questionTokens,
      bootstrapKnowledge,
      options,
      endCallback,
    } = input;

    const chatModel = this.getChatModelInstance(input.options);

    const { stream, handlers } = LangChainStream();
    const customHandleLLMEnd = async (_output: any, runId: string) => {
      await endCallback(_output.generations[0][0].text);
      return await handlers.handleLLMEnd(_output, runId);
    };

    const customHandlers = {
      ...handlers,
      handleLLMEnd: customHandleLLMEnd,
    };

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
        The user date and time is ${date}. Output format is markdown. Open links in new tabs.
        Here are more details about your character:\n
        ${ai.instructions}
        Answer questions using this knowledge:\n
      `;
    const instructionTokens = getTokenLength(engineeredPrompt);
    const chatHistoryTokens = getTokenLength(JSON.stringify(historyMessages));
    const chatSeedTokens = getTokenLength(JSON.stringify(historySeed));
    const remainingTokens =
      aiModel.contextSize -
      answerTokens -
      chatHistoryTokens -
      chatSeedTokens -
      instructionTokens -
      questionTokens -
      BUFFER_TOKENS;
    let knowledge;
    if (
      bootstrapKnowledge &&
      bootstrapKnowledge.blobUrl &&
      remainingTokens > bootstrapKnowledge.totalTokenCount
    ) {
      const resp = await axios.get(bootstrapKnowledge.blobUrl);
      if (resp.status === 200) {
        knowledge = resp.data;
      }
    }
    if (!knowledge) {
      const vectorKnowledge = await pineconeAdapter.getKnowledge(
        prompt,
        messages,
        ai.dataSources,
        remainingTokens
      );
      knowledge = vectorKnowledge.knowledge;
      knowledgeMeta = vectorKnowledge.docMeta;
    }
    const chatLog = [new SystemMessage(`${engineeredPrompt}${knowledge}\n`)];
    chatLog.push(...historySeed);
    chatLog.push(...historyMessages);

    endKnowledge = performance.now();
    chatModel.call(chatLog, {}, [customHandlers]);
    return stream;
  }
}
