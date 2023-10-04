export interface InvitationEntity {
    id?: string;
    email: string;
    inviteeUserId: string;
    workspaceId: string;
    isAccepted?: boolean;
}