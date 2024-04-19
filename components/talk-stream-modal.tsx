"use client";

import { useTalkModal } from "@/hooks/use-talk-modal";
import { startSession } from "@/src/lib/d-id";
import { X } from "lucide-react";
import { useEffect, useRef, useState } from "react";
import Draggable from "react-draggable";

import {
  Select,
  SelectContent,
  SelectItem,
  SelectTrigger,
  SelectValue,
} from "@/components/ui/select";
import { voices } from "./ai-models";

export const TalkStreamModal = () => {
  const talkModal = useTalkModal();
  const ref = useRef<HTMLVideoElement>(null);
  const [voice, setVoice] = useState("en-US-JennyNeural");
  const [isMounted, setIsMounted] = useState(false);

  useEffect(() => {
    setIsMounted(true);
  }, []);

  useEffect(() => {
    if (talkModal.ai) {
      startSession(talkModal.ai.src, ref.current);
    }
  }, [talkModal.ai]);

  if (!isMounted || !talkModal.isOpen) {
    return null;
  }

  return (
    <Draggable handle=".handle">
      <div className="fixed top-8 right-8 w-64 handle bg-ring/10">
        <X
          className="absolute top-2 right-2 h-6 w-6 cursor-pointer hover:text-ring z-20"
          onClick={talkModal.onClose}
        />
        <video
          ref={ref}
          src={talkModal.src}
          className="rounded-lg object-cover"
        >
          <p>
            Your browser doesn&apos;t support HTML video. Here is a
            <a href={talkModal.src}>link to the video</a> instead.
          </p>
        </video>
        <Select value={talkModal.voice} onValueChange={talkModal.onVoiceChange}>
          <SelectTrigger className="bg-background">
            <SelectValue defaultValue={voice} placeholder="Select a voice" />
          </SelectTrigger>
          <SelectContent>
            {voices.map((model) => (
              <SelectItem key={model.id} value={model.id}>
                {model.name} Voice
              </SelectItem>
            ))}
          </SelectContent>
        </Select>
      </div>
    </Draggable>
  );
};
