import { OrganizationMembershipRole } from "@clerk/nextjs/server";

export interface CreateOrganizationInvitationRequest {
  organizationId: string;
  inviterUserId: string;
  invitations: OrganizationInvitation[];
}

export interface OrganizationInvitation {
  emailAddress: string;
  role: OrganizationMembershipRole;
}
