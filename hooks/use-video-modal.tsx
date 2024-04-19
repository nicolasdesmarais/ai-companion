import { speak } from "@/src/lib/d-id";
import { create } from "zustand";

interface useVideoModalStore {
  isOpen: boolean;
  src: string;
  voice: string;
  ai?: any;
  onOpen: (src: string) => void;
  onOpenStream: (ai: any) => void;
  onSpeak: (text: string) => void;
  onClose: () => void;
  onVoiceChange: (voice: string) => void;
}

export const useVideoModal = create<useVideoModalStore>((set, get) => ({
  isOpen: false,
  src: "",
  voice: "en-US-JennyNeural",
  onOpen: (src) => set({ isOpen: true, src }),
  onOpenStream: (ai) => set({ isOpen: true, ai }),
  onSpeak: (text) => speak(text, get().voice),
  onClose: () => set({ isOpen: false }),
  onVoiceChange: (voice: string) => set({ voice }),
}));
