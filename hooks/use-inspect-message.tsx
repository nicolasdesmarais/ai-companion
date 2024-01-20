import { create } from "zustand";

interface useInspectMessageStore {
  isOpen: boolean;
  ai: any;
  message: any;
  messages: any[];
  query: string;
  onOpen: (ai: any, message: any, messages: any[], query: string) => void;
  onClose: () => void;
}

export const useInspectMessage = create<useInspectMessageStore>((set) => ({
  isOpen: false,
  ai: null,
  message: null,
  messages: [],
  query: "",
  onOpen: (ai: any, message: any, messages: any[], query: string) =>
    set({ isOpen: true, ai, message, messages, query }),
  onClose: () => set({ isOpen: false }),
}));
