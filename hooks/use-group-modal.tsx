import { GroupSummaryDto } from "@/src/domain/ports/api/GroupsApi";
import { create } from "zustand";

interface useGroupModalStore {
  isOpen: boolean;
  data?: GroupSummaryDto[];
  groupId?: string;
  onOpen: (groupId?: string) => void;
  onClose: () => void;
  onUpdate: (data: GroupSummaryDto[]) => void;
  onUpdateGroup: (
    updatedGroup: GroupSummaryDto,
    data?: GroupSummaryDto[]
  ) => void;
}

export const useGroupModal = create<useGroupModalStore>((set) => ({
  isOpen: false,
  onOpen: (groupId?: string) => set({ isOpen: true, groupId }),
  onClose: () => set({ isOpen: false }),
  onUpdate: (data: GroupSummaryDto[]) => {
    set({ data, isOpen: false, groupId: undefined });
  },
  onUpdateGroup: (updatedGroup: GroupSummaryDto, data?: GroupSummaryDto[]) => {
    const newData = data?.map((group) => {
      if (group.id === updatedGroup.id) {
        return updatedGroup;
      }
      return group;
    });
    set({ data: newData, isOpen: false, groupId: undefined });
  },
}));
