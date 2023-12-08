import { ChatDto, GetChatsResponse } from "@/src/domain/ports/api/ChatsApi";
import { create } from "zustand";

interface useChatsStore {
  chats: ChatDto[];
  fetchChats: () => void;
}

export const useChats = create<useChatsStore>((set) => ({
  chats: [],
  setChats: (chats: any) => set({ chats }),
  fetchChats: async () => {
    const response = await fetch("/api/v1/me/chats");
    if (response.status === 200) {
      const body: GetChatsResponse = await response.json();
      set({ chats: body.data });
    }
  },
}));
