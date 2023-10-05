import { currentUser } from "@clerk/nextjs";
import { NextResponse } from "next/server";
import { InvitationService } from '../../../../../domain/services/InvitationService'

export async function GET(req: Request) {
  try {
    const user = await currentUser();
    if (!user?.id) {
      return new NextResponse("Unauthorized", { status: 401 });
    }

    const invitationService = new InvitationService();
    const invitations = await invitationService.findInvitationsByExternalUserId(user.id)

    return NextResponse.json(invitations);
  } catch (error) {
    console.log("[WORKSPACE_POST]", error);
    return new NextResponse("Internal Error", { status: 500 });
  }
};

export async function POST(req: Request) {
  try {
    const invitationRequest: CreateInvitationRequest = await req.json();
    const user = await currentUser();

    if (!user?.id) {
      return new NextResponse("Unauthorized", { status: 401 });
    }

    const invitationService = new InvitationService();
    const invitation = invitationService.create(invitationRequest, user.id);
    return NextResponse.json(invitation);
  } catch (error) {
    console.log("[INVITATION_POST]", error);
    return new NextResponse("Internal Error", { status: 500 });
  }
};