import { clerkClient } from "@clerk/nextjs";
import { CreateOrganizationInvitationRequest } from "../types/CreateOrganizationInvitationRequest";

export class InvitationService {
  public async createInvitations(emails: string[]) {
    for (const emailAddress of emails) {
      await clerkClient.invitations.createInvitation({
        emailAddress,
      });
    }
  }

  public async createOrganizationInvitations(
    request: CreateOrganizationInvitationRequest
  ) {
    for (const invitation of request.invitations) {
      await clerkClient.organizations.createOrganizationInvitation({
        organizationId: request.organizationId,
        inviterUserId: request.inviterUserId,
        emailAddress: invitation.emailAddress,
        role: invitation.role,
      });
    }
  }
}
