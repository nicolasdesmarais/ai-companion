import { GroupAvailability } from "@prisma/client";

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
