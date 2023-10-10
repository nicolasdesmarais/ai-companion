import { auth } from "@clerk/nextjs";
import { NextResponse } from "next/server";
import { GroupService } from "../../../../../domain/services/GroupService";
import { CreateGroupRequest } from "../../../../../domain/types/CreateGroupRequest";

export async function GET(req: Request) {
  try {
    const authentication = await auth();
    const orgId = authentication.orgId;
    const userId = authentication.userId;
    if (!userId) {
      return new NextResponse("Unauthorized", { status: 401 });
    }

    const groupService = new GroupService();
    const groups = await groupService.findGroupsByUser(orgId, userId);

    return NextResponse.json(groups);
  } catch (error) {
    console.log("Error in [GET v1/me/groups]", error);
    return new NextResponse("Internal Error", { status: 500 });
  }
}

export async function POST(req: Request) {
  try {
    const authentication = await auth();
    if (!authentication?.userId || !authentication?.orgId) {
      return new NextResponse("Unauthorized", { status: 401 });
    }

    const createGroupRequest: CreateGroupRequest = await req.json();

    const groupService = new GroupService();
    const group = await groupService.createGroup(
      authentication.orgId,
      authentication.userId,
      createGroupRequest
    );

    return NextResponse.json(group);
  } catch (error) {
    console.log("[POST v1/me/groups]", error);
    return new NextResponse("Internal Error", { status: 500 });
  }
}
