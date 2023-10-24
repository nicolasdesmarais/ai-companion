import { InvitationService } from "@/src/domain/services/InvitationService";
import { Utilities } from "@/src/domain/util/utilities";
import { auth } from "@clerk/nextjs";
import { NextResponse } from "next/server";

export async function POST(req: Request) {
  try {
    const invitationRequest: CreateInvitationRequest = await req.json();
    const { userId, orgId } = await auth();
    if (!userId) {
      return new NextResponse("Unauthorized", { status: 401 });
    }
    const validEmails = Utilities.parseEmailCsv(invitationRequest.emails);

    if (validEmails.length === 0) {
      return new NextResponse("No valid emails", { status: 400 });
    }

    const invitationService = new InvitationService();
    let invitation;
    if (orgId) {
      invitation = await invitationService.createOrganizationInvitations({
        organizationId: orgId,
        inviterUserId: userId,
        invitations: validEmails.map((email) => {
          return {
            emailAddress: email,
            role: "basic_member",
          };
        }),
      });
    } else {
      invitation = await invitationService.createInvitations(validEmails);
    }
    return NextResponse.json("OK");
  } catch (error) {
    console.log("[INVITATION_POST]", error);
    if (error.errors && error.errors.length > 0 && error.errors[0].message) {
      return new NextResponse(error.errors[0].message, { status: 400 });
    }
    return new NextResponse("Internal Error", { status: 500 });
  }
}
