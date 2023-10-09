import prismadb from "@/lib/prismadb";
import { clerkClient } from '@clerk/nextjs';
import { AIVisibility, GroupAvailability, PrismaClient } from "@prisma/client";
import { ListAIsRequestParams, ListAIsRequestScope } from "./dtos/ListAIsRequestParams";

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

    public async findAIsForUser(orgId: string | undefined, userId: string, request: ListAIsRequestParams) {
        const scope = request.scope || ListAIsRequestScope.ALL;
        const orgIdFilter = orgId || '';

        const whereCondition = { AND: [{}] };
        whereCondition.AND.push(this.getBaseWhereCondition(orgIdFilter, userId, scope));

        if (request.groupId) {
            whereCondition.AND.push(this.getGroupCriteria(orgIdFilter, request.groupId));
        }
        if (request.categoryId) {
            whereCondition.AND.push(this.getCategoryCriteria(request.categoryId));
        }
        if (request.search) {
            whereCondition.AND.push(this.getSearchCriteria(request.search));
        }

        const prisma = new PrismaClient({
            log: ['query', 'info', 'warn', 'error'],
          })

        return prisma.companion.findMany({
            where: whereCondition
        });
    }

    private getBaseWhereCondition(orgId: string, userId: string, scope: ListAIsRequestScope) {
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
            case ListAIsRequestScope.GROUP:
                baseWhereCondition = this.getUserGroupCriteria(orgId, userId);
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
                baseWhereCondition.OR.push(this.getUserGroupCriteria(orgId, userId));
                baseWhereCondition.OR.push(this.getSharedWithUserCriteria(userId));
                baseWhereCondition.OR.push(this.getPublicCriteria());
                break;
        }

        return baseWhereCondition;
    }

    private getOwnedByUserCriteria(userId: string) {
        return { userId: userId };
    }

    private getUserGroupCriteria(orgId: string, userId: string) {
        return {
                visibility: {
                    in: [AIVisibility.GROUP, AIVisibility.PUBLIC]
                },
                groups: {
                    some: {
                        group: {
                            orgId: orgId,
                            OR: [
                                { availability: GroupAvailability.EVERYONE },
                                {users: {
                                    some: {
                                        userId: userId
                                    }
                                }}
                            ]
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

    private getGroupCriteria(orgId: string, groupId: string) {
        return {
            groups: {
                some: {
                    group: {
                        id: groupId,
                        orgId: orgId
                    }
                }
            }
         }
    }

    private getCategoryCriteria(categoryId: string) {
        return { categoryId: categoryId } ;
    }

    private getSearchCriteria(search: string) {
        return {
            OR: [
                {
                    name: {
                        search: search
                    }
                },
                {
                    userName: {
                        search: search
                    }
                }
            ]};
    }
}
