import prismadb from "@/lib/prismadb";
import { clerkClient } from '@clerk/nextjs';
import { GroupAvailability } from "@prisma/client";
import { CreateGroupRequest } from "../apiInterfaces/CreateGroupRequest";

export class GroupService {
    public async findGroupsByUserAndOrgId(userId: string, orgId: string){
        return await prismadb.group.findMany({
            where: {
                orgId: orgId,
                OR: [
                    {
                        availability: GroupAvailability.EVERYONE
                    },
                    {
                        users: {
                            some: {
                                userId: userId
                            }
                        }
                    }
                ]
            }
        });
    }

    public async createGroup(orgId: string, createByUserId: string, createGroupRequest: CreateGroupRequest) {
        const group = await prismadb.group.create({
            data: {
                orgId: orgId,
                name: createGroupRequest.name,
                availability: createGroupRequest.availability
            }
        });


        if (createGroupRequest.availability === GroupAvailability.RESTRICTED) {
            // Create explicit permissions for users when group availability is SELECT
            const memberEmailsArray = createGroupRequest.memberEmails.split(",").map((email) => email.trim());
            const uniqueMemberEmailsSet = new Set(memberEmailsArray);

            const clerkUserList = await clerkClient.users.getUserList({
                emailAddress: Array.from(uniqueMemberEmailsSet)
            });

            const userIds = clerkUserList.map(user => user.id);
            userIds.push(createByUserId);

            const groupUsers = userIds.map(userId => ({
                groupId: group.id,
                userId: userId
            }));

            await prismadb.groupUser.createMany({ data: groupUsers });
        }

        return group;
    }
}
