import { AI, Chat } from "@prisma/client";
import { create } from "zustand";

interface useChatsStore {
  chats: (Chat & {
    ai: AI;
  })[];
  fetchChats: () => void;
}

export const useChats = create<useChatsStore>((set) => ({
  chats: [],
  setChats: (chats: any) => set({ chats }),
  fetchChats: async () => {
    const response = await fetch("/api/v1/conversations");
    if (response.status === 200) {
      const data = await response.json();
      set({ chats: data });
    }
  },
}));
