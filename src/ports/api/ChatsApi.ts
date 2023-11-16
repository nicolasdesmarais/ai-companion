export interface GetChatsResponse {
  chats: Chat[];
}

export interface Chat {
  id: string;
  createdAt: Date;
  updatedAt: Date;
  name: string;
  aiId: string;
  userId: string;
  pinPosition: number | null;
}
