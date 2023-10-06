import prismadb from "@/lib/prismadb";


export class AIService {

    public async findAIsForUser(userId: string, request: ListAIsRequestParams) {
        let whereCondition = this.getBaseWhereCondition(userId);

        return prismadb.companion.findMany({
            where: whereCondition
        });
    }

    private getBaseWhereCondition(userId: string) {
        return {
            OR: [
                { visibility: 'public'},
                { userId: userId },
                { AND: [
                    { visibility: 'workspace'},
                    { workspaces: {
                        some: {
                            workspace: {
                                users: {
                                    some: {
                                        userId: userId
                                        }
                                    }
                            }
                        }
                    }}
                ]},
                {
                    permissions: {
                        some: {
                            userId: userId
                        }
                    }
                }
            ]
        };
    }
}