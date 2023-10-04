import prismadb from "@/lib/prismadb";
import { InvitationEntity } from "../entities/InvitationEntity";
import { clerkClient } from '@clerk/nextjs';

export class InvitationService {

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
}