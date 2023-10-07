import { auth, currentUser } from "@clerk/nextjs";
import { NextResponse } from "next/server";

import prismadb from "@/lib/prismadb";

export async function POST(req: Request) {
  try {
    const body = await req.json();
    const user = await currentUser();
    const { src, name, description, instructions, seed, categoryId, modelId, knowledge } = body;

    if (!user || !user.id) {
      return new NextResponse("Unauthorized", { status: 401 });
    }

    if (!src || !name || !description || !instructions || !categoryId) {
      return new NextResponse("Missing required fields", { status: 400 });
    };


    const companion = await prismadb.companion.create({
      data: {
        categoryId,
        userId: user.id,
        userName: user.firstName || user.username || "user",
        src,
        name,
        description,
        instructions,
        seed,
        modelId,
      }
    });

    if (knowledge && knowledge.length > 0) {
      knowledge.forEach(async (item: { id: string; }) => {
        await prismadb.knowledgeAI.create({
          data: {
            companionId: companion.id,
            knowledgeId: item.id,
          }
        });
      });
    }

    return NextResponse.json(companion);
  } catch (error) {
    console.log("[COMPANION_POST]", error);
    return new NextResponse("Internal Error", { status: 500 });
  }
};