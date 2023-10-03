import { auth } from "@clerk/nextjs";
import { NextResponse } from "next/server";
import { OpenAI } from "langchain/llms/openai";


const openai = new OpenAI({
  openAIApiKey: process.env.OPENAI_API_KEY,
  modelName: "gpt-4"
});

export async function POST(
  req: Request
) {
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

    const resp = String(
      await openai
          .call(prompt)
          .catch(console.error)
    );

    const cleaned = resp.replaceAll(",", "");
    const chunks = cleaned.split("\n");
    const response = chunks[0];

    return NextResponse.json(response);
  } catch (error) {
    console.error('[GENERATE_ERROR]', error);
    return new NextResponse("Internal Error", { status: 500 });
  }
};
