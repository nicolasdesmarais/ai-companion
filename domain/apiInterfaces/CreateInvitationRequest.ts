interface CreateInvitationRequest {
    invitations: WorkspaceInvitation[];
}

interface WorkspaceInvitation {
    email: string;
    workspaceId: string;
}