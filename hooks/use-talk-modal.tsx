import { speak } from "@/src/lib/d-id";
import { create } from "zustand";

interface useTalkModalStore {
  isOpen: boolean;
  src: string;
  ai?: any;
  onOpen: (src: string) => void;
  onOpenStream: (ai: any) => void;
  onSpeak: (text: string) => void;
  onClose: () => void;
}

export const useTalkModal = create<useTalkModalStore>((set) => ({
  isOpen: false,
  src: "",
  onOpen: (src) => set({ isOpen: true, src }),
  onOpenStream: (ai) => set({ isOpen: true, ai }),
  onSpeak: (text) => speak(text),
  onClose: () => set({ isOpen: false }),
}));
