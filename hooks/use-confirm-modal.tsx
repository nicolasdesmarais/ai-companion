import { create } from "zustand";
import { ReactElement } from "react";

interface useConfirmModalStore {
  isOpen: boolean;
  title?: ReactElement | string;
  body?: ReactElement | string;
  onOpen: (
    title: ReactElement | string,
    body: ReactElement | string,
    onConfirm: () => void
  ) => void;
  onClose: () => void;
  onConfirm: () => void;
}

export const useConfirmModal = create<useConfirmModalStore>((set) => ({
  isOpen: false,
  title: "Are you sure?",
  body: "This action cannot be undone.",
  onConfirm: () => {},
  onOpen: (title, body, onConfirm) =>
    set({ isOpen: true, title, body, onConfirm }),
  onClose: () => set({ isOpen: false }),
}));
