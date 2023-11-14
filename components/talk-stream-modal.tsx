"use client";

import { useEffect, useState, useRef } from "react";
import { X } from "lucide-react";
import { useTalkModal } from "@/hooks/use-talk-modal";
import Draggable from "react-draggable";
import { ta } from "date-fns/locale";
import { startSession } from "@/src/lib/d-id";

export const TalkStreamModal = () => {
  const talkModal = useTalkModal();
  const ref = useRef<HTMLVideoElement>(null);
  const [isMounted, setIsMounted] = useState(false);

  useEffect(() => {
    setIsMounted(true);
  }, []);

  useEffect(() => {
    console.log("talkModal.ai", talkModal.ai);
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
      </div>
    </Draggable>
  );
};
