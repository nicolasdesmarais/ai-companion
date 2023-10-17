import { StreamingTextResponse, LangChainStream } from "ai";
import { currentUser } from "@clerk/nextjs";
import { Replicate } from "langchain/llms/replicate";
import { OpenAI } from "langchain/llms/openai";
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

    let model;
    if (conversation.companion.modelId === "llama2-13b") {
      model = new Replicate({
        model:
          "meta/llama-2-13b-chat:f4e2de70d66816a838a89eeeb621910adffb0dd0baba3976c96980970978018d",
        input: {
          max_length: 2048,
        },
        apiKey: process.env.REPLICATE_API_TOKEN,
        callbackManager: CallbackManager.fromHandlers(handlers),
      });
    } else {
      model = new OpenAI({
        openAIApiKey: process.env.OPENAI_API_KEY,
        modelName: "gpt-4",
        maxTokens: -1,
      });
    }

    // Turn verbose on for debugging
    model.verbose = true;

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
    const engineeredPrompt = `
      ONLY generate plain sentences without prefix of who is speaking. DO NOT use ${conversation.companion.name}: prefix. 
      ${conversation.companion.instructions}
      Below are relevant details about ${conversation.companion.name}'s past and the conversation you are in.
      ${knowledge}\n
      ${seededChatHistory}\n
      ${conversation.companion.name}:
    `;
    console.log(engineeredPrompt);

    const resp = await model.call(engineeredPrompt);

    let response;
    if (conversation.companion.modelId === "llama2-13b") {
      const cleaned = resp.replaceAll(",", "");
      response = cleaned;
    } else {
      response = resp;
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
