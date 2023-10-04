import { auth, currentUser } from "@clerk/nextjs";
import { NextResponse } from "next/server";
import { WorkspaceService } from '../../../../../domain/services/WorkspaceService'

export async function GET(req: Request) {
  try {
    const user = await currentUser();
    if (!user || !user.id) {
      return new NextResponse("Unauthorized", { status: 401 });
    }

    const workspaceService = new WorkspaceService();
    const workspaces = await workspaceService.getWorkspacesByExternalUserId(user.id)

    return NextResponse.json(workspaces);
  } catch (error) {
    console.log("[WORKSPACE_POST]", error);
    return new NextResponse("Internal Error", { status: 500 });
  }
};