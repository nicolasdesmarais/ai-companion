import { create } from "zustand";
import { Group } from "@prisma/client";

interface useGroupModalStore {
  isOpen: boolean;
  data?: Group[];
  onOpen: () => void;
  onClose: () => void;
  onUpdate: (data: Group[]) => void;
}

export const useGroupModal = create<useGroupModalStore>((set) => ({
  isOpen: false,
  onOpen: () => set({ isOpen: true }),
  onClose: () => set({ isOpen: false }),
  onUpdate: (data: Group[]) => {
    set({ data, isOpen: false });
  },
}));
