export interface GroupUserEntity {
  groupId: string;
  userId: string | null;
  email: string;
  createdAt: Date;
  updatedAt: Date;
}
