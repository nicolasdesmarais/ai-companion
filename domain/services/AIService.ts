import prismadb from "@/lib/prismadb";


export class AIService {

    public async findAIsForUser(externalUserId: string, request: ListAIsRequestParams) {
        let whereCondition = this.getBaseWhereCondition(externalUserId);

        return prismadb.companion.findMany({
            where: whereCondition
        });
    }

    private getBaseWhereCondition(externalUserId: string) {
        return {
            OR: [
                { visibility: 'public'},
                { userId: externalUserId },
                { AND: [
                    { visibility: 'workspace'},
                    { workspaces: {
                        some: {
                            workspace: {
                                users: {
                                    some: {
                                        userId: externalUserId
                                        }
                                    }
                            }
                        }
                    }}
                ]}
            ]
        };
    }
}