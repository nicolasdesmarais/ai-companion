import { AI, Chat } from "@prisma/client";
import { create } from "zustand";

interface useChatsStore {
  chats: (Chat & {
    ai: AI;
  })[];
  fetchConversations: () => void;
}

export const useChats = create<useChatsStore>((set) => ({
  chats: [],
  setConversations: (conversations: any) => set({ chats: conversations }),
  fetchConversations: async () => {
    const response = await fetch("/api/v1/conversations");
    if (response.status === 200) {
      const data = await response.json();
      set({ chats: data });
    }
  },
}));
