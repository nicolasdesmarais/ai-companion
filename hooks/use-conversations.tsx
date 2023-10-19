import { create } from "zustand";
import { Companion, Conversation } from "@prisma/client";

interface useConversationsStore {
  conversations: (Conversation & {
    companion: Companion;
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
