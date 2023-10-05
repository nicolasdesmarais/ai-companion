import prismadb from "@/lib/prismadb";
import { InvitationEntity } from "../entities/InvitationEntity";
import { clerkClient } from '@clerk/nextjs';
import { WorkspaceService } from "./WorkspaceService";

export class InvitationService {

    public async findInvitationById(invitationId: string) {
        return prismadb.invitation.findUnique({
            where: {
                id: invitationId
            }
        });
    }

    public async create(invitationRequest: CreateInvitationRequest, inviteeUserId: string) {
        const invitationEntity: InvitationEntity = {
            email: invitationRequest.email,
            workspaceId: invitationRequest.workspaceId,
            inviteeUserId: inviteeUserId
        }

        await prismadb.invitation.create({data: invitationEntity});

        clerkClient.invitations.createInvitation({
            emailAddress: invitationRequest.email
        });
    }

    public async acceptInvitation(invitation: InvitationEntity, invitedUserExternalId: string) {
        const workspaceService = new WorkspaceService();
        await workspaceService.addUserToWorkspace(invitedUserExternalId, invitation.workspaceId);
        prismadb.invitation.update({ where: { id: invitation.id }, data: { isAccepted: true } })
    }
}