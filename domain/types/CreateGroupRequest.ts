import { GroupAvailability } from "@prisma/client";

export interface CreateGroupRequest {
    name: string;
    availability: GroupAvailability;
    memberEmails: string; // A comma-separate list of emails of users who should be added to the group
}
