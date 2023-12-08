import { create } from "zustand";

interface useRateAIStore {
  isOpen: boolean;
  onOpen: () => void;
  onClose: () => void;
}

export const useRateAI = create<useRateAIStore>((set) => ({
  isOpen: false,
  onOpen: () => set({ isOpen: true }),
  onClose: () => set({ isOpen: false }),
}));
