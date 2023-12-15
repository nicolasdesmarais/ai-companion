import { clerkClient } from "@clerk/nextjs";
import {
  CreateOrganizationInvitationRequest,
  OrganizationInvitation,
} from "../../adapter-in/api/InvitationsApi";

const DEFAULT_INVITATION_ROLE = "basic_member";

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

  public async createOrganizationInvitationsFromEmails(
    orgId: string,
    userId: string,
    emails: string[]
  ) {
    const orgInvitations: OrganizationInvitation[] = [];
    emails.forEach((email) => {
      const invitation: OrganizationInvitation = {
        emailAddress: email,
        role: DEFAULT_INVITATION_ROLE,
      };
      orgInvitations.push(invitation);
    });

    const createInvitationRequest: CreateOrganizationInvitationRequest = {
      organizationId: orgId,
      inviterUserId: userId,
      invitations: orgInvitations,
    };

    await this.createOrganizationInvitations(createInvitationRequest);
  }
}

const invitationService = new InvitationService();
export default invitationService;
