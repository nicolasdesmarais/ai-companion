import { UserEntity } from "../entities/UserEntity";
import { WorkspaceEntity } from "../entities/WorkspaceEntity";
import prismadb from "@/lib/prismadb";

export class WorkspaceService {

    public async addUserToNewOrExistingWorkspace(user: UserEntity){
        if (!user.id) {
            return;
        }

        const domain = this.extractDomain(user.email);
        if (domain === null) {
            console.log('Cannot extract domain from email address: ' + user.email)
            return;
        }

        const isPublicDomain = await this.isPublicDomain(domain);
        if (isPublicDomain) {
            console.log('Domain is public: ' + domain)
            return;
        }

        const existingWorkspace = await prismadb.workspace.findFirst({
            where: {
                domain: domain
            }
        });

        if (existingWorkspace !== null) {
            // Workspace already exists
            // Add user to workspace
            await this.addUserToWorkspace(user.id, existingWorkspace.id);
        } else {
            // Create workspace
            const createdWorkspace = await this.createWorkspace(domain);
            await this.addUserToWorkspace(user.id, createdWorkspace.id);
        }
    }

    async createWorkspace(domain: string) {
        return prismadb.workspace.create({
            data: {
                name: domain,
                domain: domain
            }
        });
    }


    async addUserToWorkspace(userId: string, workspaceId: string) {
        return prismadb.workspaceUser.create({
            data: {
                userId: userId,
                workspaceId: workspaceId
            }
        });
    }

    private extractDomain(email: string): string | null {
        const parts = email.split('@');
        return parts.length > 1 ? parts[1] : null;
    }

    private async isPublicDomain(domain: string) {
        const publicDomain = await prismadb.publicDomain.findFirst({
            where: {
                domain: domain
            }
        });

        return publicDomain !== null;

    }
}