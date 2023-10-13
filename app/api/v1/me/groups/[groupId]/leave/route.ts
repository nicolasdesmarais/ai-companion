import { BadRequestError, EntityNotFoundError } from "@/domain/errors/Errors";
import { GroupService } from "@/domain/services/GroupService";
import { auth } from "@clerk/nextjs";
import { NextResponse } from "next/server";

export async function PUT(
  req: Request,
  { params }: { params: { groupId: string } }
) {
  try {
    const authentication = await auth();
    if (!authentication?.userId || !authentication?.orgId) {
      return new NextResponse("Unauthorized", { status: 401 });
    }

    const groupService = new GroupService();
    await groupService.leaveGroup(
      authentication.orgId,
      authentication.userId,
      params.groupId
    );

    const groups = await groupService.findGroupsByUser(
      authentication.orgId,
      authentication.userId
    );
    return NextResponse.json(groups);
  } catch (error) {
    if (error instanceof EntityNotFoundError) {
      return new NextResponse("Group not found", { status: 404 });
    }
    if (error instanceof BadRequestError) {
      return new NextResponse(error.message, { status: 400 });
    }

    console.log("[PUT v1/me/groups/[groupId]/leave]", error);
    return new NextResponse("Internal Error", { status: 500 });
  }
}
