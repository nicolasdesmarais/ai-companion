import { auth, currentUser } from "@clerk/nextjs";
import { NextResponse } from "next/server";

import prismadb from "@/lib/prismadb";

export async function PATCH(
  req: Request,
  { params }: { params: { companionId: string } }
) {
  try {
    const body = await req.json();
    const user = await currentUser();
    const { src, name, description, instructions, seed, categoryId, modelId, knowledge } = body;

    if (!params.companionId) {
      return new NextResponse("Companion ID required", { status: 400 });
    }

    if (!user || !user.id) {
      return new NextResponse("Unauthorized", { status: 401 });
    }

    if (!src || !name || !description || !instructions || !categoryId) {
      return new NextResponse("Missing required fields", { status: 400 });
    };


    const companion = await prismadb.companion.update({
      where: {
        id: params.companionId,
        userId: user.id,
      },
      include: {
        knowledge: true,
      },
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
        if (!companion.knowledge.find((k: { knowledgeId: string; }) => k.knowledgeId === item.id)) {
          await prismadb.knowledgeAI.create({
            data: {
              companionId: companion.id,
              knowledgeId: item.id,
            }
          });
        }
      });
    }

    return NextResponse.json(companion);
  } catch (error) {
    console.log("[COMPANION_PATCH]", error);
    return new NextResponse("Internal Error", { status: 500 });
  }
};

export async function DELETE(
  request: Request,
  { params }: { params: { companionId: string } }
) {
  try {
    const { userId } = auth();

    if (!userId) {
      return new NextResponse("Unauthorized", { status: 401 });
    }

    const companion = await prismadb.companion.delete({
      where: {
        userId,
        id: params.companionId
      }
    });

    return NextResponse.json(companion);
  } catch (error) {
    console.log("[COMPANION_DELETE]", error);
    return new NextResponse("Internal Error", { status: 500 });
  }
};