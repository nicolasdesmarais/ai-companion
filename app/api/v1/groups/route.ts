import { auth } from "@clerk/nextjs";
import { NextResponse } from "next/server";
import { GroupService } from "../../../../domain/services/GroupService";
import { CreateGroupRequest } from "../../../../domain/types/CreateGroupRequest";

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
    console.log("[POST v1/groups]", error);
    return new NextResponse("Internal Error", { status: 500 });
  }
}
