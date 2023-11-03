import { auth, currentUser } from "@clerk/nextjs";
import { NextResponse } from "next/server";

import aiService from "@/src/domain/services/AIService";
import prismadb from "@/src/lib/prismadb";
import groupService from "@/src/domain/services/GroupService";

export async function PATCH(
  req: Request,
  { params }: { params: { aiId: string } }
) {
  try {
    const body = await req.json();
    const user = await currentUser();
    const {
      src,
      name,
      description,
      instructions,
      seed,
      categoryId,
      modelId,
      groups,
      visibility,
      options,
    } = body;

    if (!params.aiId) {
      return new NextResponse("AI ID required", { status: 400 });
    }

    if (!user?.id) {
      return new NextResponse("Unauthorized", { status: 401 });
    }

    if (!src || !name || !description || !instructions || !categoryId) {
      return new NextResponse("Missing required fields", { status: 400 });
    }

    const ai = await prismadb.aI.update({
      where: {
        id: params.aiId,
        userId: user.id,
      },
      include: {
        dataSources: {
          include: {
            dataSource: true,
          },
        },
        groups: true,
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
        visibility,
        options,
      },
    });

    if (visibility !== "GROUP") {
      await groupService.updateAIGroups(ai.id, []);
      ai.groups = [];
    } else if (groups && groups.length > 0) {
      await groupService.updateAIGroups(ai.id, groups);
      ai.groups = groups;
    }

    return NextResponse.json(ai);
  } catch (error) {
    console.log("[COMPANION_PATCH]", error);
    return new NextResponse("Internal Error", { status: 500 });
  }
}

export async function DELETE(
  request: Request,
  { params }: { params: { aiId: string } }
) {
  try {
    const { userId, orgId } = auth();

    if (!userId || !orgId) {
      return new NextResponse("Unauthorized", { status: 401 });
    }

    await aiService.deleteAI(orgId, userId, params.aiId);

    return new NextResponse(null, { status: 204 });
  } catch (error) {
    console.log("[COMPANION_DELETE]", error);
    return new NextResponse("Internal Error", { status: 500 });
  }
}
