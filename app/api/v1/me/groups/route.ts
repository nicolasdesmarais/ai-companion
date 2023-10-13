import { auth } from "@clerk/nextjs";
import { NextResponse } from "next/server";
import { GroupService } from "../../../../../domain/services/GroupService";

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
