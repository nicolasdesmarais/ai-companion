import { ReactElement } from "react";
import { create } from "zustand";

interface useConfirmModalStore {
  isOpen: boolean;
  title?: ReactElement | string;
  body?: ReactElement | string;
  footer?: ReactElement | string;
  loading?: boolean;
  onOpen: (
    title: ReactElement | string,
    body: ReactElement | string,
    onConfirm: () => void,
    footer?: ReactElement | string
  ) => void;
  onClose: () => void;
  onConfirm: () => void;
  onLoading: (loading: boolean) => void;
}

export const useConfirmModal = create<useConfirmModalStore>((set) => ({
  isOpen: false,
  title: "Are you sure?",
  body: "This action cannot be undone.",
  onLoading: (loading) => set({ loading }),
  onConfirm: () => {},
  onOpen: (title, body, onConfirm, footer) =>
    set({ isOpen: true, title, body, onConfirm, footer }),
  onClose: () => set({ isOpen: false, loading: false }),
}));
