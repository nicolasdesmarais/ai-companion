import { auth, currentUser } from "@clerk/nextjs";
import { NextResponse } from "next/server";
import { WorkspaceService } from '../../../domain/services/WorkspaceService'

import prismadb from "@/lib/prismadb";

export async function POST(req: Request) {
  try {
    const body = await req.json();
    const user = await currentUser();
    const { name } = body;

    if (!user || !user.id) {
      return new NextResponse("Unauthorized", { status: 401 });
    }

    const workspaceService = new WorkspaceService();
    const workspace = workspaceService.createWorkspace(user.id, name);

    return NextResponse.json(workspace);
  } catch (error) {
    console.log("[WORKSPACE_POST]", error);
    return new NextResponse("Internal Error", { status: 500 });
  }
};