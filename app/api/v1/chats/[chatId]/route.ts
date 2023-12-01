import { CreateChatRequest } from "@/src/domain/ports/api/ChatsApi";
import aiModelService from "@/src/domain/services/AIModelService";
import chatService from "@/src/domain/services/ChatService";
import { AuthorizationScope } from "@/src/domain/types/AuthorizationContext";
import { MemoryManager } from "@/src/lib/memory";
import { rateLimit } from "@/src/lib/rate-limit";
import { getTokenLength } from "@/src/lib/tokenCount";
import { withAuthorization } from "@/src/middleware/AuthorizationMiddleware";
import { withErrorHandler } from "@/src/middleware/ErrorMiddleware";
import { Message, Role } from "@prisma/client";
import { JsonObject } from "@prisma/client/runtime/library";
import { LangChainStream, StreamingTextResponse } from "ai";
import axios from "axios";
import { CallbackManager } from "langchain/callbacks";
import { ChatOpenAI } from "langchain/chat_models/openai";
import { OpenAI } from "langchain/llms/openai";
import { Replicate } from "langchain/llms/replicate";
import { HumanMessage, SystemMessage } from "langchain/schema";
import { NextResponse } from "next/server";
import prismadb from "@/src/lib/prismadb";

// small buffer so we don't go over the limit
const BUFFER_TOKENS = 200;

export const maxDuration = 300;

const getKnowledge = async (
  prompt: string,
  history: Message[],
  dataSources: any[],
  availTokens: number
) => {
  if (dataSources.length === 0) {
    return { knowledge: "", docMeta: [] };
  }

  const knowledgeIds: string[] = dataSources
    .map((ds) => ds.dataSource.knowledges.map((k: any) => k.knowledgeId))
    .reduce((acc, curr) => acc.concat(curr), []);

  const { totalDocs, totalTokens } = dataSources.reduce(
    (dsAcc, ds) => {
      const { docs, tokens } = ds.dataSource.knowledges.reduce(
        (acc: any, k: any) => {
          if (
            k.knowledge.metadata &&
            k.knowledge.metadata.totalTokenCount &&
            k.knowledge.metadata.documentCount
          ) {
            acc.tokens += k.knowledge.metadata.totalTokenCount;
            acc.docs += k.knowledge.metadata.documentCount;
            return acc;
          } else {
            return { docs: NaN, tokens: NaN };
          }
        },
        { docs: 0, tokens: 0 }
      );
      dsAcc.totalDocs += docs;
      dsAcc.totalTokens += tokens;
      return dsAcc;
    },
    { totalDocs: 0, totalTokens: 0 }
  );
  const docDensity = totalTokens / totalDocs;
  let estDocsNeeded = Math.ceil(availTokens / docDensity) || 100;
  estDocsNeeded = Math.min(10000, Math.max(estDocsNeeded, 1));

  let query = prompt;
  if (history.length > 1) {
    query = `${history[history.length - 2].content}\n${query}`;
  }
  if (history.length > 2) {
    query = `${history[history.length - 3].content}\n${query}`;
  }

  const memoryManager = await MemoryManager.getInstance();
  const similarDocs = await memoryManager.vectorSearch(
    query,
    knowledgeIds,
    estDocsNeeded
  );

  let knowledge = "",
    docMeta = [];
  if (!!similarDocs && similarDocs.length !== 0) {
    for (let i = 0; i < similarDocs.length; i++) {
      const doc = similarDocs[i];
      const newKnowledge = `${knowledge}\n${doc.pageContent}`;
      const newKnowledgeTokens = getTokenLength(newKnowledge);
      if (newKnowledgeTokens < availTokens) {
        knowledge = newKnowledge;
        docMeta.push(doc.metadata);
      } else {
        break;
      }
    }
  }
  return { knowledge, docMeta };
};

async function postHandler(
  request: Request,
  context: { params: { chatId: string }; orgId: string; userId: string }
) {
  const start = performance.now();
  let endSetup = start,
    endKnowledge = start,
    end = start,
    knowledgeMeta: any;

  const { params, userId } = context;
  const chatId = params.chatId;

  const chatRequest: CreateChatRequest = await request.json();
  const { date, prompt, aiId, messages } = chatRequest;

  const identifier = request.url + "-" + userId;
  const { success } = await rateLimit(identifier);

  if (!success) {
    return new NextResponse("Rate limit exceeded", { status: 429 });
  }

  let chat: any;
  if (chatId === "test-chat") {
    if (!aiId) {
      return new NextResponse("AI not found", { status: 404 });
    }
    chat = await chatService.getTestChat(aiId, userId, messages || [], prompt);
  } else {
    chat = await chatService.updateChat(chatId, userId, prompt, Role.user);
  }

  if (!chat) {
    return new NextResponse("Conversation not found", { status: 404 });
  }

  const model = await aiModelService.findAIModelById(chat.ai.modelId);

  if (!model) {
    return new NextResponse(`Model ${chat.ai.modelId} not found`, {
      status: 404,
    });
  }

  const { stream, handlers } = LangChainStream();

  const customHandleLLMEnd = async (_output: any, runId: string) => {
    if (chatId !== "test-chat") {
      end = performance.now();
      const answer = _output.generations[0][0].text;
      const setupTime = Math.round(endSetup - start);
      const knowledgeTime = Math.round(endKnowledge - endSetup);
      const llmTime = Math.round(end - endKnowledge);
      const totalTime = Math.round(end - start);
      await chatService.updateChat(chatId, userId, answer, Role.system, {
        setupTime,
        knowledgeTime,
        llmTime,
        totalTime,
        knowledgeMeta,
      });
    }
    return await handlers.handleLLMEnd(_output, runId);
  };

  const customHandlers = {
    ...handlers,
    handleLLMEnd: customHandleLLMEnd,
  };

  let completionModel,
    chatModel,
    options = {} as any;

  Object.entries(chat.ai.options || {}).forEach(([key, value]) => {
    if (value && value.length > 0) {
      options[key] = value[0];
    }
  });

  if (chat.ai.modelId === "llama2-13b") {
    completionModel = new Replicate({
      model:
        "meta/llama-2-13b-chat:f4e2de70d66816a838a89eeeb621910adffb0dd0baba3976c96980970978018d",
      input: {
        ...options,
        top_p: options.topP,
        max_tokens: options.maxTokens,
      },
      apiKey: process.env.REPLICATE_API_TOKEN,
      callbackManager: CallbackManager.fromHandlers(customHandlers),
    });
  } else if (chat.ai.modelId === "text-davinci-003") {
    completionModel = new OpenAI({
      openAIApiKey: process.env.OPENAI_API_KEY,
      modelName: "text-davinci-003",
      ...options,
    });
  } else if (chat.ai.modelId === "gpt35-16k") {
    chatModel = new ChatOpenAI({
      azureOpenAIApiKey: process.env.AZURE_GPT35_KEY,
      azureOpenAIApiVersion: "2023-05-15",
      azureOpenAIApiInstanceName: "appdirect-prod-ai-useast",
      azureOpenAIApiDeploymentName: "ai-prod-16k",
      streaming: true,
      ...options,
    });
  } else {
    chatModel = new ChatOpenAI({
      azureOpenAIApiKey: process.env.AZURE_GPT40_KEY,
      azureOpenAIApiVersion: "2023-05-15",
      azureOpenAIApiInstanceName: "prod-appdirectai-east2",
      azureOpenAIApiDeploymentName: "gpt4-32k",
      streaming: true,
      ...options,
    });
  }

  const questionTokens = getTokenLength(prompt);
  const answerTokens = ((chat.ai.options as JsonObject)?.maxTokens ||
    model.options.maxTokens.default) as number;

  let bootstrapKnowledge;
  if (chat.ai.dataSources.length === 1) {
    if (chat.ai.dataSources[0].dataSource.knowledges.length === 1) {
      const meta = chat.ai.dataSources[0].dataSource.knowledges[0].knowledge
        .metadata as any;
      if (
        meta &&
        meta.mimeType &&
        meta.totalTokenCount &&
        meta.mimeType === "text/plain"
      ) {
        bootstrapKnowledge = {
          ...chat.ai.dataSources[0].dataSource.knowledges[0].knowledge,
          ...meta,
        };
      }
    }
  }

  endSetup = performance.now();
  if (completionModel) {
    const chatHistory = chat.messages.reduce(
      (acc: string, message: Message) => {
        if (message.role === "user") {
          return acc + `User: ${message.content}\n`;
        } else {
          return acc + `${chat.ai.name}: ${message.content}\n`;
        }
      },
      ""
    );
    const completionPrompt = `
        ONLY generate plain sentences without prefix of who is speaking. DO NOT use ${chat.ai.name}: prefix.
        The user date and time is ${date}. Output format is markdown. Open links in new tabs.
        ${chat.ai.instructions}
        Below are relevant details about ${chat.ai.name}'s past and the conversation you are in:\n
      `;
    const instructionTokens = getTokenLength(completionPrompt);
    const seededChatHistory = `${chat.ai.seed}\n\n${chatHistory}\n${chat.ai.name}:`;
    const chatHistoryTokens = getTokenLength(seededChatHistory);
    const remainingTokens =
      model.contextSize -
      answerTokens -
      chatHistoryTokens -
      instructionTokens -
      questionTokens -
      BUFFER_TOKENS;
    const { knowledge, docMeta } = await getKnowledge(
      prompt,
      chat.messages,
      chat.ai.dataSources,
      remainingTokens
    );
    knowledgeMeta = docMeta;
    endKnowledge = performance.now();
    let response = await completionModel.call(
      `${completionPrompt}${knowledge}${seededChatHistory}`
    );
    response = response.replaceAll(",", "");
    var Readable = require("stream").Readable;
    let s = new Readable();
    s.push(response);
    s.push(null);
    return new StreamingTextResponse(s);
  } else {
    let historySeed = [];
    if (chat.ai.seed) {
      historySeed = chat.ai.seed
        .split("\n\n")
        .reduce((result: any, line: string) => {
          if (line.trimStart().startsWith(chat.ai.name)) {
            result.push(
              new SystemMessage(
                line.replace(chat.ai.name + ":", "").trimStart()
              )
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
    const historyMessages = chat.messages.map((message) => {
      if (message.role === "user") {
        return new HumanMessage(message.content);
      } else {
        return new SystemMessage(message.content);
      }
    });
    const engineeredPrompt = `
        Pretend you are ${chat.ai.name}, ${chat.ai.description}.
        The user date and time is ${date}. Output format is markdown. Open links in new tabs.
        Here are more details about your character:\n
        ${chat.ai.instructions}
        Answer questions using this knowledge:\n
      `;
    const instructionTokens = getTokenLength(engineeredPrompt);
    const chatHistoryTokens = getTokenLength(JSON.stringify(historyMessages));
    const chatSeedTokens = getTokenLength(JSON.stringify(historySeed));
    const remainingTokens =
      model.contextSize -
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
      const vectorKnowledge = await getKnowledge(
        prompt,
        chat.messages,
        chat.ai.dataSources,
        remainingTokens
      );
      knowledge = vectorKnowledge.knowledge;
      knowledgeMeta = vectorKnowledge.docMeta;
    }
    const chatLog = [new SystemMessage(`${engineeredPrompt}${knowledge}\n`)];
    chatLog.push(...historySeed);
    chatLog.push(...historyMessages);
    if (!chatModel) {
      return new NextResponse("Missing chat model", { status: 500 });
    }
    endKnowledge = performance.now();
    chatModel.call(chatLog, {}, [customHandlers]);
    return new StreamingTextResponse(stream);
  }
}

async function deleteHandler(
  request: Request,
  context: { params: { chatId: string }; orgId: string; userId: string }
) {
  const { params, userId } = context;

  chatService.deleteChat(params.chatId, userId);

  return new NextResponse(null, { status: 204 });
}

export const POST = withErrorHandler(
  withAuthorization(AuthorizationScope.CHATS_WRITE, postHandler)
);

export const DELETE = withErrorHandler(
  withAuthorization(AuthorizationScope.CHATS_WRITE, deleteHandler)
);
