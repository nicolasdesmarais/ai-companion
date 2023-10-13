export interface GroupUserEntity {
  groupId: string;
  userId: string | null;
  email: string | null;
  createdAt: Date;
  updatedAt: Date;
}
