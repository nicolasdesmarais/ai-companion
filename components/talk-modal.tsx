"use client";

import { useEffect, useState, useRef } from "react";
import { X } from "lucide-react";
import { useTalkModal } from "@/hooks/use-talk-modal";
import Draggable from "react-draggable";

export const TalkModal = () => {
  const talkModal = useTalkModal();
  const [isMounted, setIsMounted] = useState(false);
  const ref = useRef<HTMLVideoElement>(null);

  useEffect(() => {
    setIsMounted(true);
  }, []);

  useEffect(() => {
    if (ref.current) {
      ref.current.load();
      ref.current.addEventListener("ended", () => {
        ref.current.currentTime = 0;
      });
    }
  }, [talkModal.src]);

  useEffect(() => {
    if (ref.current?.ended) {
    }
  }, [ref.current?.ended]);

  if (!isMounted || !talkModal.isOpen) {
    return null;
  }

  return (
    <Draggable handle=".handle">
      <div className="fixed top-8 right-8 w-64 handle">
        <X
          className="absolute top-2 right-2 h-6 w-6 cursor-pointer hover:text-ring z-20"
          onClick={talkModal.onClose}
        />
        <video
          ref={ref}
          src={talkModal.src}
          autoPlay
          onClick={() => ref.current?.play()}
          className="rounded-lg object-cover"
        >
          <p>
            Your browser doesn't support HTML video. Here is a
            <a href={talkModal.src}>link to the video</a> instead.
          </p>
        </video>
      </div>
    </Draggable>
  );
};
