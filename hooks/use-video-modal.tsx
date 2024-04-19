import { speak } from "@/src/lib/d-id";
import { create } from "zustand";

interface useVideoModalStore {
  isOpen: boolean;
  voice: string;
  videoId?: string;
  ai?: any;
  onOpenStream: (ai: any) => void;
  onSpeak: (text: string) => void;
  onClose: () => void;
  onVoiceChange: (voice: string) => void;
}

export const useVideoModal = create<useVideoModalStore>((set, get) => ({
  isOpen: false,
  videoId: "",
  voice: "en-US-JennyNeural",
  onOpenStream: (ai) => set({ isOpen: true, ai }),
  onSpeak: (text) => speak(text, get().voice),
  onClose: () => set({ isOpen: false }),
  onVoiceChange: (voice: string) => set({ voice }),
}));
