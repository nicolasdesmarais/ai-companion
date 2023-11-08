import { create } from "zustand";

interface useTalkModalStore {
  isOpen: boolean;
  src: string;
  onOpen: (src: string) => void;
  onClose: () => void;
}

export const useTalkModal = create<useTalkModalStore>((set) => ({
  isOpen: false,
  src: "",
  onOpen: (src) => set({ isOpen: true, src }),
  onClose: () => set({ isOpen: false }),
}));
