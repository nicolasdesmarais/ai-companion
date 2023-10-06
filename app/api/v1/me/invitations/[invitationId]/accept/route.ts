import { currentUser } from "@clerk/nextjs";
import { NextResponse } from "next/server";
import { InvitationService } from "@/domain/services/InvitationService";

export async function PUT(
  req: Request,
  { params }: { params: { invitationId: string } }
) {
  try {
    const user = await currentUser();

    if (!params.invitationId) {
      return new NextResponse("Invitation ID required", { status: 400 });
    }

    if (!user?.id) {
      return new NextResponse("Unauthorized", { status: 401 });
    }

    const invitationService = new InvitationService();
    const invitationEntity = await invitationService.findInvitationById(params.invitationId);
    if (!invitationEntity) {
      return new NextResponse("Invitation not found", { status: 404 });
    }

    if (!user.emailAddresses.some((emailAddress: any) => emailAddress.email === invitationEntity.email)) {
      return new NextResponse("Unauthorized", { status: 401 });
  }

    await invitationService.acceptInvitation(invitationEntity, user.id);

    return new NextResponse("", { status: 200 })
  } catch (error) {
    console.log("ERROR in [v1/me/invitations/{invitationId}/accept]]]", error);
    return new NextResponse("Internal Error", { status: 500 });
  }
};