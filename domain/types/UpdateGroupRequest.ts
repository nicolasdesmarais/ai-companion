import { GroupAvailability } from "@prisma/client";

export interface UpdateGroupRequest {
    name: string;
    availability: GroupAvailability;
    memberEmailsToAdd: string; // A comma-separated list of emails of users who should be added to the group
    userIdsToRemove: string[]; // A comma-separated list of IDs of users who should be removed from the group
}
