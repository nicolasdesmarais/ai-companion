import prismadb from "@/lib/prismadb";
import { clerkClient } from "@clerk/nextjs";
import { InvitationEntity } from "../entities/InvitationEntity";
import { CreateOrganizationInvitationRequest } from "../types/CreateOrganizationInvitationRequest";
import { WorkspaceService } from "./WorkspaceService";

export class InvitationService {
  public async findInvitationById(invitationId: string) {
    return prismadb.invitation.findUnique({
      where: {
        id: invitationId,
      },
    });
  }

  public async findInvitationsByEmailAddresses(emailAddresses: string[]) {
    return prismadb.invitation.findMany({
      where: {
        email: {
          in: emailAddresses,
        },
      },
    });
  }

  public async createInvitations(emails: string[]) {
    for (const emailAddress of emails) {
      await clerkClient.invitations.createInvitation({
        emailAddress,
      });
    }
  }

  public async acceptInvitation(
    invitation: InvitationEntity,
    invitedUserId: string
  ) {
    const workspaceService = new WorkspaceService();
    await workspaceService.addUserToWorkspace(
      invitedUserId,
      invitation.workspaceId
    );
    prismadb.invitation.update({
      where: { id: invitation.id },
      data: { isAccepted: true },
    });
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
