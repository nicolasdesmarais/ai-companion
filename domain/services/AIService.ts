import prismadb from "@/lib/prismadb";
import { clerkClient } from '@clerk/nextjs';
import { ListAIsRequestParams, ListAIsRequestScope } from "./dtos/ListAIsRequestParams";

enum AIVisibility {
    PRIVATE = 'PRIVATE',
    WORKSPACE = 'WORKSPACE',
    PUBLIC = 'PUBLIC'
}

export class AIService {

    public async findAIById(id: string) {
        return prismadb.companion.findUnique({
            where: {
                id: id
            }
        });
    }

    public async shareAi(aiId: string, request: ShareAIRequest) {
        const userEmails = request.users.map(user => user.email);

        const clerkUserList = await clerkClient.users.getUserList({ emailAddress: userEmails });
        const aiPermissions = clerkUserList.map(user => ({
            userId: user.id,
            companionId: aiId
        }));

        prismadb.aIPermissions.createMany({ data: aiPermissions });
    }

    public async findAIsForUser(userId: string, request: ListAIsRequestParams) {
        const scope = request.scope || ListAIsRequestScope.ALL;
        const whereCondition = this.getBaseWhereCondition(userId, scope);

        return prismadb.companion.findMany({
            where: whereCondition
        });
    }

    private getBaseWhereCondition(userId: string, scope: ListAIsRequestScope ) {
        let baseWhereCondition;

        switch(scope) {
            case ListAIsRequestScope.PRIVATE:
                baseWhereCondition = { AND: [{}] };
                baseWhereCondition.AND.push({ visibility: AIVisibility.PRIVATE });
                baseWhereCondition.AND.push(this.getOwnedByUserCriteria(userId));
                break;
            case ListAIsRequestScope.OWNED:
                baseWhereCondition = this.getOwnedByUserCriteria(userId);
                break;
            case ListAIsRequestScope.WORKSPACE:
                baseWhereCondition = this.getUserWorkspaceCriteria(userId);
                break;
            case ListAIsRequestScope.SHARED:
                baseWhereCondition = this.getSharedWithUserCriteria(userId);
                break;
            case ListAIsRequestScope.PUBLIC:
                baseWhereCondition = this.getPublicCriteria();
                break;
            case ListAIsRequestScope.ALL:
                baseWhereCondition = { OR: [{}] };
                baseWhereCondition.OR.push(this.getOwnedByUserCriteria(userId));
                baseWhereCondition.OR.push(this.getUserWorkspaceCriteria(userId));
                baseWhereCondition.OR.push(this.getSharedWithUserCriteria(userId));
                baseWhereCondition.OR.push(this.getPublicCriteria());
                break;
        }

        return baseWhereCondition;
    }

    private getOwnedByUserCriteria(userId: string) {
        return { userId: userId };
    }

    private getUserWorkspaceCriteria(userId: string) {
        return {
            visibility: {
                in: [AIVisibility.WORKSPACE, AIVisibility.PUBLIC],
            },
            workspaces: {
                some: {
                    workspace: {
                        users: {
                            some: {
                                userId: userId
                            }
                        }
                   }
                }
            }
        }
    }

    private getSharedWithUserCriteria(userId: string) {
        return {
             permissions: {
                some: {
                    userId: userId
                }
            }
        };
    }

    private getPublicCriteria() {
        return { visibility: AIVisibility.PUBLIC };
    }
}