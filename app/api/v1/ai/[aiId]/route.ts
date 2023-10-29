import { auth, currentUser } from "@clerk/nextjs";
import { NextResponse } from "next/server";

import prismadb from "@/src/lib/prismadb";

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
      knowledge,
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
    const { userId } = auth();

    if (!userId) {
      return new NextResponse("Unauthorized", { status: 401 });
    }

    const ai = await prismadb.aI.delete({
      where: {
        userId,
        id: params.aiId,
      },
    });

    return NextResponse.json(ai);
  } catch (error) {
    console.log("[COMPANION_DELETE]", error);
    return new NextResponse("Internal Error", { status: 500 });
  }
}
