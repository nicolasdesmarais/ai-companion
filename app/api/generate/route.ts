import { auth } from "@clerk/nextjs";
import { NextResponse } from "next/server";
import { ChatOpenAI } from "langchain/chat_models/openai";
import { SystemChatMessage } from "langchain/schema";

export const maxDuration = 300;

const openai = new ChatOpenAI({
  azureOpenAIApiKey: process.env.AZURE_GPT40_KEY,
  azureOpenAIApiVersion: "2023-05-15",
  azureOpenAIApiInstanceName: "prod-appdirectai-east2",
  azureOpenAIApiDeploymentName: "gpt4-32k",
});

export async function POST(req: Request) {
  try {
    const { userId } = auth();
    const body = await req.json();
    const { prompt } = body;

    if (!userId) {
      return new NextResponse("Unauthorized", { status: 401 });
    }

    if (!prompt) {
      return new NextResponse("Prompt is required", { status: 400 });
    }

    const resp = await openai.call([new SystemChatMessage(prompt)]);

    return NextResponse.json(resp.text);
  } catch (error) {
    console.error("[GENERATE_ERROR]", error);
    return new NextResponse("Internal Error", { status: 500 });
  }
}
