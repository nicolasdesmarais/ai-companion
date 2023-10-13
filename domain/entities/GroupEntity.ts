import { GroupAvailability } from "@prisma/client";
import { GroupUserEntity } from "./GroupUserEntity";

export interface GroupEntity {
  id: string;
  createdAt: Date;
  updatedAt: Date;
  orgId: string;
  name: string;
  ownerUserId: string;
  availability: GroupAvailability;
  users?: GroupUserEntity[];
}
