import { create } from "zustand";
import { Group } from "@prisma/client";

interface useGroupModalStore {
  isOpen: boolean;
  data?: Group[];
  groupId?: string;
  onOpen: (groupId?: string) => void;
  onClose: () => void;
  onUpdate: (data: Group[]) => void;
}

export const useGroupModal = create<useGroupModalStore>((set) => ({
  isOpen: false,
  onOpen: (groupId?: string) => set({ isOpen: true, groupId }),
  onClose: () => set({ isOpen: false }),
  onUpdate: (data: Group[]) => {
    set({ data, isOpen: false, groupId: undefined });
  },
}));
