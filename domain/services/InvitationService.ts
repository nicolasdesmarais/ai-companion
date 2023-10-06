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

    public async findInvitationsByEmailAddresses(emailAddresses: string[]) {
        return prismadb.invitation.findMany({
            where: {
                email: {
                    in: emailAddresses
                }
            }
        });
    }

    public async createInvitations(invitationRequest: CreateInvitationRequest, inviteeUserId: string) {
        const invitationEntities = invitationRequest.invitations.map(invitationRequest => ({
            email: invitationRequest.email,
            inviteeUserId: inviteeUserId,
            workspaceId: invitationRequest.workspaceId,
        }));

        await prismadb.invitation.createMany({data: invitationEntities});

        for (const invitation of invitationRequest.invitations) {
            // Creating invitation with clerkClient
            await clerkClient.invitations.createInvitation({
                emailAddress: invitation.email
            });
        }
    }

    public async acceptInvitation(invitation: InvitationEntity, invitedUserId: string) {
        const workspaceService = new WorkspaceService();
        await workspaceService.addUserToWorkspace(invitedUserId, invitation.workspaceId);
        prismadb.invitation.update({ where: { id: invitation.id }, data: { isAccepted: true } })
    }
}