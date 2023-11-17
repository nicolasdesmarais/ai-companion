import { auth, currentUser } from "@clerk/nextjs";
import { NextResponse } from "next/server";
import groupService from "@/src/domain/services/GroupService";
import EmailUtils from "@/src/lib/emailUtils";
import prismadb from "@/src/lib/prismadb";

export async function POST(req: Request) {
  try {
    const body = await req.json();
    const authentication = await auth();
    const orgId = authentication.orgId;
    const user = await currentUser();
    const {
      src,
      name,
      description,
      instructions,
      seed,
      categoryId,
      modelId,
      visibility,
      options,
      groups,
    } = body;

    if (!user?.id || !orgId) {
      return new NextResponse("Unauthorized", { status: 401 });
    }

    if (!src || !name || !description || !instructions || !categoryId) {
      return new NextResponse("Missing required fields", { status: 400 });
    }

    const emailAddress = EmailUtils.getUserPrimaryEmailAddress(user);

    const ai = await prismadb.aI.create({
      data: {
        categoryId,
        orgId,
        userId: user.id,
        userName: emailAddress ?? user.firstName ?? user.username ?? "user",
        src,
        name,
        description,
        instructions,
        seed: seed ?? "",
        modelId,
        visibility,
        options,
      },
      include: {
        dataSources: {
          include: {
            dataSource: true,
          },
        },
        groups: true,
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
    console.log("[COMPANION_POST]", error);
    return new NextResponse("Internal Error", { status: 500 });
  }
}
