import { StreamingTextResponse, LangChainStream } from "ai";
import { currentUser } from "@clerk/nextjs";
import { Replicate } from "langchain/llms/replicate";
import { OpenAI } from "langchain/llms/openai";
import { ChatOpenAI } from "langchain/chat_models/openai";
import { HumanChatMessage, SystemChatMessage } from "langchain/schema";
import { CallbackManager } from "langchain/callbacks";
import { NextResponse } from "next/server";

import { MemoryManager } from "@/lib/memory";
import { rateLimit } from "@/lib/rate-limit";
import prismadb from "@/lib/prismadb";
import { Message } from "@prisma/client";

export const maxDuration = 300;

export async function POST(
  request: Request,
  {
    params: { aiId, conversationId },
  }: { params: { aiId: string; conversationId: string } }
) {
  try {
    const { prompt } = await request.json();
    const user = await currentUser();

    if (!user || !user.id) {
      return new NextResponse("Unauthorized", { status: 401 });
    }

    const identifier = request.url + "-" + user.id;
    const { success } = await rateLimit(identifier);

    if (!success) {
      return new NextResponse("Rate limit exceeded", { status: 429 });
    }

    const conversation = await prismadb.conversation.update({
      where: {
        id: conversationId,
      },
      include: {
        companion: {
          include: {
            knowledge: true,
          },
        },
        messages: {
          orderBy: {
            createdAt: "asc",
          },
        },
      },
      data: {
        messages: {
          create: {
            content: prompt,
            role: "user",
            userId: user.id,
            companionId: aiId,
          },
        },
      },
    });

    if (!conversation) {
      return new NextResponse("Conversation not found", { status: 404 });
    }

    const memoryManager = await MemoryManager.getInstance();

    const knowledgeIds = conversation.companion.knowledge.map(
      (item) => item.knowledgeId
    );
    const similarDocs = await memoryManager.vectorSearch(prompt, knowledgeIds);
    let knowledge = "";
    if (!!similarDocs && similarDocs.length !== 0) {
      knowledge = similarDocs.map((doc) => doc.pageContent).join("\n");
    }
    const { handlers } = LangChainStream();

    let completionModel, chatModel;
    if (conversation.companion.modelId === "llama2-13b") {
      completionModel = new Replicate({
        model:
          "meta/llama-2-13b-chat:f4e2de70d66816a838a89eeeb621910adffb0dd0baba3976c96980970978018d",
        input: {
          max_length: 2048,
        },
        apiKey: process.env.REPLICATE_API_TOKEN,
        callbackManager: CallbackManager.fromHandlers(handlers),
      });
    } else if (conversation.companion.modelId === "text-davinci-003") {
      completionModel = new OpenAI({
        openAIApiKey: process.env.OPENAI_API_KEY,
        modelName: "text-davinci-003",
        maxTokens: -1,
      });
    } else if (conversation.companion.modelId === "gpt35-16k") {
      chatModel = new ChatOpenAI({
        azureOpenAIApiKey: process.env.AZURE_GPT35_KEY,
        azureOpenAIApiVersion: "2023-05-15",
        azureOpenAIApiInstanceName: "appdirect-prod-ai-useast",
        azureOpenAIApiDeploymentName: "ai-prod-16k",
      });
    } else {
      chatModel = new ChatOpenAI({
        azureOpenAIApiKey: process.env.AZURE_GPT40_KEY,
        azureOpenAIApiVersion: "2023-05-15",
        azureOpenAIApiInstanceName: "prod-appdirectai-east2",
        azureOpenAIApiDeploymentName: "gpt4-32k",
      });
    }

    const chatHistory = conversation.messages.reduce(
      (acc: string, message: Message) => {
        if (message.role === "user") {
          return acc + `User: ${message.content}\n`;
        } else {
          return acc + `${conversation.companion.name}: ${message.content}\n`;
        }
      },
      ""
    );
    const seededChatHistory = `${conversation.companion.seed}\n\n${chatHistory}`;
    const completionPrompt = `
      ONLY generate plain sentences without prefix of who is speaking. DO NOT use ${conversation.companion.name}: prefix. 
      Output format is markdown. Open links in new tabs.
      ${conversation.companion.instructions}
      Below are relevant details about ${conversation.companion.name}'s past and the conversation you are in.
      ${knowledge}\n
      ${seededChatHistory}\n
      ${conversation.companion.name}:
    `;

    const engineeredPrompt = `
      Pretend you are ${conversation.companion.name}, ${conversation.companion.description}. 
      Output format is markdown. Open links in new tabs.
      Here are more details about your character:\n 
      ${conversation.companion.instructions} 
      Answer questions using this knowledge:\n
      ${knowledge}\n
    `;

    let response;
    if (completionModel) {
      const resp = await completionModel.call(completionPrompt);
      const cleaned = resp.replaceAll(",", "");
      response = cleaned;
    } else {
      const chatLog = [new SystemChatMessage(engineeredPrompt)];
      const convertedMessages = conversation.messages.map((message) => {
        if (message.role === "user") {
          return new HumanChatMessage(message.content);
        } else {
          return new SystemChatMessage(message.content);
        }
      });
      chatLog.push(...convertedMessages);
      if (!chatModel) {
        return new NextResponse("Missing chat model", { status: 500 });
      }
      const chatResponse = await chatModel.call(chatLog);
      response = chatResponse.text;
    }

    var Readable = require("stream").Readable;

    let s = new Readable();
    s.push(response);
    s.push(null);
    if (response !== undefined && response.length > 1) {
      await prismadb.conversation.update({
        where: {
          id: conversationId,
        },
        data: {
          messages: {
            create: {
              content: response.trim(),
              role: "system",
              userId: user.id,
              companionId: aiId,
            },
          },
        },
      });
    }

    return new StreamingTextResponse(s);
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
