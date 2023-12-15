import { GroupAvailability } from "@prisma/client";

export interface CreateGroupRequest {
  name: string;
  availability: GroupAvailability;
  memberEmails: string; // A comma-separated list of emails of users who should be added to the group
}

export interface UpdateGroupRequest {
  name: string;
  availability: GroupAvailability;
  memberEmailsToAdd: string; // A comma-separated list of emails of users who should be added to the group
  memberEmailsToRemove: string[]; // A comma-separated list of IDs of users who should be removed from the group
}

export interface GroupSummaryDto {
  id: string;
  createdAt: Date;
  updatedAt: Date;
  orgId: string;
  ownerUserId: string;
  name: string;
  availability: GroupAvailability;
}

export interface GroupDetailDto extends GroupSummaryDto {
  users: GroupUser[];
}

export interface GroupUser {
  createdAt: Date;
  updatedAt: Date;
  userId: string | null;
  email: string;
}
