import {
  ChatSummaryDto,
  ListChatsResponse,
} from "@/src/domain/ports/api/ChatsApi";
import { create } from "zustand";

interface useChatsStore {
  chats: ChatSummaryDto[];
  loading: boolean;
  fetchChats: () => void;
}

export const useChats = create<useChatsStore>((set) => ({
  chats: [],
  setChats: (chats: ChatSummaryDto[]) => set({ chats }),
  loading: true,
  fetchChats: async () => {
    set({ loading: true });
    const response = await fetch("/api/v1/me/chats");
    if (response.status === 200) {
      const body: ListChatsResponse = await response.json();
      set({ chats: body.data });
    }
    set({ loading: false });
  },
}));
