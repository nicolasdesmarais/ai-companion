import { CreateGroupRequest } from "@/src/ports/api/GroupsApi";
import { auth } from "@clerk/nextjs";
import { NextResponse } from "next/server";
import { GroupService } from "../../../../src/domain/services/GroupService";

export async function POST(req: Request) {
  try {
    const authentication = await auth();
    if (!authentication?.userId || !authentication?.orgId) {
      return new NextResponse("Unauthorized", { status: 401 });
    }

    const createGroupRequest: CreateGroupRequest = await req.json();

    const groupService = new GroupService();
    await groupService.createGroup(
      authentication.orgId,
      authentication.userId,
      createGroupRequest
    );

    const groups = await groupService.findGroupsByUser(
      authentication.orgId,
      authentication.userId
    );
    return NextResponse.json(groups);
  } catch (error) {
    console.log("[POST v1/groups]", error);
    return new NextResponse("Internal Error", { status: 500 });
  }
}

export async function GET(req: Request) {
  try {
    const { userId, orgId } = await auth();
    if (!userId) {
      return new NextResponse("Unauthorized", { status: 401 });
    }

    if (!orgId) {
      return NextResponse.json([]);
    }

    const groupService = new GroupService();
    const groups = await groupService.findGroupsByUser(orgId, userId);
    return NextResponse.json(groups);
  } catch (error) {
    console.log("[GET v1/groups]", error);
    return new NextResponse("Internal Error", { status: 500 });
  }
}
