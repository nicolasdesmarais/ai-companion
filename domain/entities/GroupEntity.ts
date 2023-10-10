import { GroupAvailability } from "@prisma/client";
import { GroupUserEntity } from "./GroupUserEntity";

export interface GroupEntity {
  id: string;
  createdAt: Date;
  updatedAt: Date;
  orgId: string;
  name: string;
  availability: GroupAvailability;
  users?: GroupUserEntity[];
}
