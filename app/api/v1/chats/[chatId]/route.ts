import { models } from "@/components/ai-models";
import {
  EntityNotFoundError,
  ForbiddenError,
} from "@/src/domain/errors/Errors";
import chatService from "@/src/domain/services/ChatService";
import { getAuthorizationContext } from "@/src/lib/authorizationUtils";
import { MemoryManager } from "@/src/lib/memory";
import { rateLimit } from "@/src/lib/rate-limit";
import { getTokenLength } from "@/src/lib/tokenCount";
import { CreateChatRequest } from "@/src/ports/api/ChatsApi";
import { Message, Role } from "@prisma/client";
import { JsonObject } from "@prisma/client/runtime/library";
import { LangChainStream, StreamingTextResponse } from "ai";
import axios from "axios";
import { CallbackManager } from "langchain/callbacks";
import { ChatOpenAI } from "langchain/chat_models/openai";
import { OpenAI } from "langchain/llms/openai";
import { Replicate } from "langchain/llms/replicate";
import { HumanChatMessage, SystemChatMessage } from "langchain/schema";
import { NextResponse } from "next/server";

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

/**
 * @swagger
 * /api/v1/chats/{chatId}:
 *   post:
 *     summary: Start a chat session
 *     description: Initiates a chat with the AI using the provided prompt on the specified date.
 *     operationId: postChatSession
 *     parameters:
 *       - name: chatId
 *         in: path
 *         required: true
 *         description: The unique identifier of the chat session.
 *         schema:
 *           type: string
 *     requestBody:
 *       required: true
 *       content:
 *         application/json:
 *           schema:
 *             type: object
 *             properties:
 *               date:
 *                 type: string
 *                 format: date-time
 *                 description: The date of the chat.
 *               prompt:
 *                 type: string
 *                 description: The prompt to use for chatting with an AI.
 *             required:
 *               - date
 *               - prompt
 *     responses:
 *       '200':
 *         description: Returns a streaming text response from the AI.
 *         content:
 *           application/json:
 *             schema:
 *               $ref: '#/components/schemas/StreamingTextResponse'
 *       '400':
 *         description: Bad request, when the request body does not contain the required fields or contains invalid data.
 *       '404':
 *         description: Not found, when the specified chat ID does not exist.
 *       '500':
 *         description: Internal Server Error, any internal error.
 * components:
 *   schemas:
 *     StreamingTextResponse:
 *       type: object
 *       properties:
 *         message:
 *           type: string
 *           description: The streaming text response from the AI.
 */
export async function POST(
  request: Request,
  { params: { chatId } }: { params: { chatId: string } }
) {
  const start = performance.now();
  let endSetup = start,
    endKnowledge = start,
    end = start,
    knowledgeMeta: any;
  try {
    const chatRequest: CreateChatRequest = await request.json();
    const { date, prompt } = chatRequest;

    const authorizationContext = await getAuthorizationContext();
    if (!authorizationContext?.orgId || !authorizationContext?.userId) {
      return new NextResponse("Unauthorized", { status: 401 });
    }
    const { userId } = authorizationContext;

    const identifier = request.url + "-" + userId;
    const { success } = await rateLimit(identifier);

    if (!success) {
      return new NextResponse("Rate limit exceeded", { status: 429 });
    }

    const chat = await chatService.updateChat(
      chatId,
      userId,
      prompt,
      Role.user
    );

    if (!chat) {
      return new NextResponse("Conversation not found", { status: 404 });
    }

    const model = models.find((model) => model.id === chat.ai.modelId);

    if (!model) {
      return new NextResponse(`Model ${chat.ai.modelId} not found`, {
        status: 404,
      });
    }

    const { stream, handlers } = LangChainStream();

    const customHandleLLMEnd = async (_output: any, runId: string) => {
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
                new SystemChatMessage(
                  line.replace(chat.ai.name + ":", "").trimStart()
                )
              );
            } else {
              result.push(
                new HumanChatMessage(
                  line.trimStart().replace("Human:", "").trimStart()
                )
              );
            }
            return result;
          }, []);
      }
      const historyMessages = chat.messages.map((message) => {
        if (message.role === "user") {
          return new HumanChatMessage(message.content);
        } else {
          return new SystemChatMessage(message.content);
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
      const chatLog = [
        new SystemChatMessage(`${engineeredPrompt}${knowledge}\n`),
      ];
      chatLog.push(...historySeed);
      chatLog.push(...historyMessages);
      if (!chatModel) {
        return new NextResponse("Missing chat model", { status: 500 });
      }
      endKnowledge = performance.now();
      chatModel.call(chatLog, {}, [customHandlers]);
      return new StreamingTextResponse(stream);
    }
  } catch (error) {
    if (error.response?.data?.error?.message) {
      console.error("[CHAT]", error.response.data.error.message);
      return new NextResponse(error.response.data.error.message, {
        status: 500,
      });
    }
    console.error("[CHAT]", error);
    return new NextResponse("Internal Error", { status: 500 });
  }
}

/**
 * @swagger
 * /api/v1/chats/{chatId}:
 *   delete:
 *     summary: Delete a chat session
 *     description: Deletes the chat session with the specified ID.
 *     operationId: deleteChat
 *     parameters:
 *       - name: chatId
 *         in: path
 *         required: true
 *         description: The unique identifier of the chat session to delete.
 *         schema:
 *           type: string
 *     responses:
 *       '204':
 *         description: Chat session successfully deleted, no content to return.
 *       '403':
 *         description: Forbidden, the user is not authorized to perform this action.
 *       '404':
 *         description: Not Found, the specified chat ID does not exist.
 *       '500':
 *         description: Internal Server Error, any internal error.
 */
export async function DELETE(
  request: Request,
  {
    params: { chatId: chatId },
  }: {
    params: { chatId: string };
  }
) {
  try {
    const authorizationContext = await getAuthorizationContext();
    if (!authorizationContext?.orgId || !authorizationContext?.userId) {
      return new NextResponse("Unauthorized", { status: 401 });
    }
    const { userId } = authorizationContext;

    chatService.deleteChat(chatId, userId);

    return new NextResponse(null, { status: 204 });
  } catch (error) {
    if (error instanceof EntityNotFoundError) {
      return new NextResponse(error.message, { status: 404 });
    }
    if (error instanceof ForbiddenError) {
      return new NextResponse(error.message, { status: 403 });
    }

    console.error("[DELETE v1/chats/[chatId]]", error);
    return new NextResponse("Internal Error", { status: 500 });
  }
}
