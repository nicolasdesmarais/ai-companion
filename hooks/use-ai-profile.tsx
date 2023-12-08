import { create } from "zustand";

interface useAIProfileStore {
  isOpen: boolean;
  onOpen: () => void;
  onClose: () => void;
}

export const useAIProfile = create<useAIProfileStore>((set) => ({
  isOpen: false,
  onOpen: () => set({ isOpen: true }),
  onClose: () => set({ isOpen: false }),
}));
