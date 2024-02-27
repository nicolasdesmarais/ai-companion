import { AIModel } from "@/src/domain/models/AIModel";
import { ChatCohere } from "@langchain/cohere";
import { HumanMessage } from "@langchain/core/messages";
import { HttpResponseOutputParser } from "langchain/output_parsers";
import { AbstractBaseChatModel } from "./AbstractBaseChatModel";
import { ChatModel, PostToChatInput } from "./ChatModel";

const MODEL_ID = "cohere";

export class CohereModel extends AbstractBaseChatModel implements ChatModel {
  public supports(model: AIModel): boolean {
    return model.id === MODEL_ID;
  }

  protected getChatModelInstance(model: AIModel, options: any, callbacks: any) {
    return new ChatCohere({
      apiKey: process.env.COHERE_API_KEY,
      model: "command",
      callbacks,
      ...options,
    });
  }

  protected async createStream(
    input: PostToChatInput
  ): Promise<ReadableStream> {
    const { ai, aiModel, date, getKnowledgeCallback } = input;

    const engineeredPrompt = super.createEngineeredPrompt(ai, date);

    const historySeed = super.createHistorySeed(ai);

    const tokensUsed = super.calculateTokensUsed([
      engineeredPrompt,
      historySeed,
    ]);

    const knowledge = await getKnowledgeCallback(tokensUsed);

    const chatLog = [new HumanMessage(`${engineeredPrompt}\n`), ...historySeed];

    const callbacks = super.getCallbacks(input);

    const chatModel = this.getChatModelInstance(
      aiModel,
      input.options,
      callbacks
    );

    const parser = new HttpResponseOutputParser();
    const streamOptions: any = {
      conversationId: input.chat.id,
      documents: knowledge.docMeta,
    };

    return await chatModel.pipe(parser).stream(chatLog, streamOptions);
  }
}
