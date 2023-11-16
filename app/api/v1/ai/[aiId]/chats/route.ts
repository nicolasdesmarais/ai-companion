import { models } from "@/components/ai-models";
import conversationService from "@/src/domain/services/ConversationService";
import { MemoryManager } from "@/src/lib/memory";
import { rateLimit } from "@/src/lib/rate-limit";
import { getTokenLength } from "@/src/lib/tokenCount";
import { CreateChatRequest } from "@/src/ports/api/ChatsApi";
import { currentUser } from "@clerk/nextjs";
import { Message } from "@prisma/client";
import { JsonObject } from "@prisma/client/runtime/library";
import { LangChainStream, StreamingTextResponse } from "ai";
import axios from "axios";
import { CallbackManager } from "langchain/callbacks";
import { ChatOpenAI } from "langchain/chat_models/openai";
import { OpenAI } from "langchain/llms/openai";
import { Replicate } from "langchain/llms/replicate";
import { HumanChatMessage, SystemChatMessage } from "langchain/schema";
import { NextRequest, NextResponse } from "next/server";

// small buffer so we don't go over the limit
const BUFFER_TOKENS = 10;

export const maxDuration = 300;

const getKnowledge = async (
  prompt: string,
  history: Message[],
  dataSources: any[],
  availTokens: number
) => {
  if (dataSources.length === 0) {
    return "";
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

  let knowledge = "";
  if (!!similarDocs && similarDocs.length !== 0) {
    for (let i = 0; i < similarDocs.length; i++) {
      const doc = similarDocs[i];
      const newKnowledge = `${knowledge}\n${doc.pageContent}`;
      const newKnowledgeTokens = getTokenLength(newKnowledge);
      if (newKnowledgeTokens < availTokens) {
        knowledge = newKnowledge;
      } else {
        break;
      }
    }
  }
  return knowledge;
};

/**
 * @swagger
 * /api/v1/ai/{aiId}/chats:
 *   post:
 *     summary: Send a message to the AI chat service
 *     description: This endpoint allows sending a prompt to an AI and optionally specifying a conversation ID for context.
 *     operationId: chatWithAI
 *     parameters:
 *       - name: aiId
 *         in: path
 *         required: true
 *         description: The identifier of the AI to chat with.
 *         schema:
 *           type: string
 *     requestBody:
 *       required: true
 *       content:
 *         application/json:
 *           schema:
 *             type: object
 *             properties:
 *               prompt:
 *                 type: string
 *                 description: The prompt message to send to the AI.
 *                 required: true
 *               conversationId:
 *                 type: string
 *                 description: An optional conversation identifier for maintaining the context of the chat.
 *     responses:
 *       '200':
 *         description: Successfully received the AI's response.
 *         content:
 *           application/json:
 *             schema:
 *               type: object
 *               properties:
 *                 message:
 *                   type: string
 *                   description: The AI's response to the prompt.
 *                 conversationId:
 *                   type: string
 *                   description: The conversation identifier.
 *       '400':
 *         description: Bad request when the request body does not contain a valid prompt.
 *       '404':
 *         description: Not Found, when the specified AI ID does not exist.
 *       '500':
 *         description: Internal Server Error
 *     security:
 *       - ApiKeyAuth: []
 */
export async function POST(
  request: Request,
  { params: { aiId } }: { params: { aiId: string } }
) {
  try {
    const chatRequest: CreateChatRequest = await request.json();
    const { conversationId, prompt } = chatRequest;

    const user = await currentUser();

    if (!user?.id) {
      return new NextResponse("Unauthorized", { status: 401 });
    }

    const identifier = request.url + "-" + user.id;
    const { success } = await rateLimit(identifier);

    if (!success) {
      return new NextResponse("Rate limit exceeded", { status: 429 });
    }

    const conversation = await conversationService.updateConversation(
      aiId,
      user.id,
      prompt,
      conversationId
    );

    if (!conversation) {
      return new NextResponse("Conversation not found", { status: 404 });
    }

    const model = models.find((model) => model.id === conversation.ai.modelId);

    if (!model) {
      return new NextResponse(`Model ${conversation.ai.modelId} not found`, {
        status: 404,
      });
    }

    const { stream, handlers } = LangChainStream();

    let completionModel,
      chatModel,
      options = {} as any;

    Object.entries(conversation.ai.options || {}).forEach(([key, value]) => {
      if (value && value.length > 0) {
        options[key] = value[0];
      }
    });

    if (conversation.ai.modelId === "llama2-13b") {
      completionModel = new Replicate({
        model:
          "meta/llama-2-13b-chat:f4e2de70d66816a838a89eeeb621910adffb0dd0baba3976c96980970978018d",
        input: {
          ...options,
          top_p: options.topP,
          max_tokens: options.maxTokens,
        },
        apiKey: process.env.REPLICATE_API_TOKEN,
        callbackManager: CallbackManager.fromHandlers(handlers),
      });
    } else if (conversation.ai.modelId === "text-davinci-003") {
      completionModel = new OpenAI({
        openAIApiKey: process.env.OPENAI_API_KEY,
        modelName: "text-davinci-003",
        ...options,
      });
    } else if (conversation.ai.modelId === "gpt35-16k") {
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
    const answerTokens = ((conversation.ai.options as JsonObject)?.maxTokens ||
      model.options.maxTokens.default) as number;

    let bootstrapKnowledge;
    if (conversation.ai.dataSources.length === 1) {
      if (conversation.ai.dataSources[0].dataSource.knowledges.length === 1) {
        const meta = conversation.ai.dataSources[0].dataSource.knowledges[0]
          .knowledge.metadata as any;
        if (
          meta &&
          meta.mimeType &&
          meta.totalTokenCount &&
          meta.mimeType === "text/plain"
        ) {
          bootstrapKnowledge = {
            ...conversation.ai.dataSources[0].dataSource.knowledges[0]
              .knowledge,
            ...meta,
          };
        }
      }
    }

    if (completionModel) {
      const chatHistory = conversation.messages.reduce(
        (acc: string, message: Message) => {
          if (message.role === "user") {
            return acc + `User: ${message.content}\n`;
          } else {
            return acc + `${conversation.ai.name}: ${message.content}\n`;
          }
        },
        ""
      );
      const completionPrompt = `
        ONLY generate plain sentences without prefix of who is speaking. DO NOT use ${conversation.ai.name}: prefix.
        Output format is markdown. Open links in new tabs.
        ${conversation.ai.instructions}
        Below are relevant details about ${conversation.ai.name}'s past and the conversation you are in:\n
      `;
      const instructionTokens = getTokenLength(completionPrompt);
      const seededChatHistory = `${conversation.ai.seed}\n\n${chatHistory}\n${conversation.ai.name}:`;
      const chatHistoryTokens = getTokenLength(seededChatHistory);
      const remainingTokens =
        model.contextSize -
        answerTokens -
        chatHistoryTokens -
        instructionTokens -
        questionTokens -
        BUFFER_TOKENS;
      const knowledge = await getKnowledge(
        prompt,
        conversation.messages,
        conversation.ai.dataSources,
        remainingTokens
      );
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
      if (conversation.ai.seed) {
        historySeed = conversation.ai.seed
          .split("\n\n")
          .reduce((result: any, line: string) => {
            if (line.trimStart().startsWith(conversation.ai.name)) {
              result.push(
                new SystemChatMessage(
                  line.replace(conversation.ai.name + ":", "").trimStart()
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
      const convertedMessages = conversation.messages.map((message) => {
        if (message.role === "user") {
          return new HumanChatMessage(message.content);
        } else {
          return new SystemChatMessage(message.content);
        }
      });
      const engineeredPrompt = `
        Pretend you are ${conversation.ai.name}, ${conversation.ai.description}.
        Output format is markdown. Open links in new tabs.
        Here are more details about your character:\n
        ${conversation.ai.instructions}
        Answer questions using this knowledge:\n
      `;
      const instructionTokens = getTokenLength(engineeredPrompt);
      const chatHistoryTokens = getTokenLength(
        JSON.stringify(convertedMessages)
      );
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
        knowledge = await getKnowledge(
          prompt,
          conversation.messages,
          conversation.ai.dataSources,
          remainingTokens
        );
      }
      const chatLog = [
        new SystemChatMessage(`${engineeredPrompt}${knowledge}\n`),
      ];
      chatLog.push(...historySeed);
      chatLog.push(...convertedMessages);
      if (!chatModel) {
        return new NextResponse("Missing chat model", { status: 500 });
      }
      chatModel.call(chatLog, {}, [handlers]);

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
 * /api/v1/ai/{aiId}/chats:
 *   get:
 *     summary: Get all chats for the AI
 *     description: Retrieves a list of all chat sessions associated with the given AI identifier.
 *     operationId: getAIChats
 *     parameters:
 *       - name: aiId
 *         in: path
 *         required: true
 *         description: The identifier of the AI whose chats are to be retrieved.
 *         schema:
 *           type: string
 *     responses:
 *       '200':
 *         description: A list of chat sessions associated with the AI.
 *         content:
 *           application/json:
 *             schema:
 *               $ref: '#/components/schemas/GetChatsResponse'
 *       '404':
 *         description: AI not found with the given identifier.
 *       '500':
 *         description: Internal Server Error
 *     security:
 *       - ApiKeyAuth: []
 * components:
 *   schemas:
 *     GetChatsResponse:
 *       type: object
 *       properties:
 *         chats:
 *           type: array
 *           items:
 *             $ref: '#/components/schemas/Chat'
 *     Chat:
 *       type: object
 *       properties:
 *         id:
 *           type: string
 *           description: Unique identifier for the chat session.
 *         createdAt:
 *           type: string
 *           format: date-time
 *           description: The date and time when the chat session was created.
 *         updatedAt:
 *           type: string
 *           format: date-time
 *           description: The date and time when the chat session was last updated.
 *         name:
 *           type: string
 *           description: Name of the chat session.
 *         aiId:
 *           type: string
 *           description: Identifier of the AI associated with the chat session.
 *         userId:
 *           type: string
 *           description: Identifier of the user associated with the chat session.
 *         pinPosition:
 *           type: integer
 *           format: int32
 *           description: The position of the chat in a pinned list or similar.
 */
export async function GET(
  request: NextRequest,
  { params }: { params: { aiId: string } }
) {
  const user = await currentUser();
  if (!user?.id) {
    return new NextResponse("Unauthorized", { status: 401 });
  }

  const chatsResponse = await conversationService.getAIConversations(
    params.aiId,
    user.id
  );
  return NextResponse.json(chatsResponse);
}
