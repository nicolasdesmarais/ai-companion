import { GroupSummaryDto } from "@/src/domain/ports/api/GroupsApi";
import { create } from "zustand";

interface UseGroupModalStore {
  isOpen: boolean;
  data?: GroupSummaryDto[];
  groupId?: string;
  areGroupsUpdated: boolean;
  onOpen: (groupId?: string) => void;
  onClose: () => void;
  onUpdate: () => void;
}

export const useGroupModal = create<UseGroupModalStore>((set) => ({
  isOpen: false,
  areGroupsUpdated: false,
  onOpen: (groupId?: string) =>
    set({ areGroupsUpdated: false, isOpen: true, groupId }),
  onClose: () => set({ isOpen: false }),
  onUpdate: () => {
    set({ areGroupsUpdated: true, isOpen: false, groupId: undefined });
  },
}));
