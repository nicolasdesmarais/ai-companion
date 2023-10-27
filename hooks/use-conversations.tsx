import { AI, Conversation } from "@prisma/client";
import { create } from "zustand";

interface useConversationsStore {
  conversations: (Conversation & {
    ai: AI;
  })[];
  fetchConversations: () => void;
}

export const useConversations = create<useConversationsStore>((set) => ({
  conversations: [],
  setConversations: (conversations: any) => set({ conversations }),
  fetchConversations: async () => {
    const response = await fetch("/api/v1/conversations");
    if (response.status === 200) {
      const data = await response.json();
      set({ conversations: data });
    }
  },
}));
