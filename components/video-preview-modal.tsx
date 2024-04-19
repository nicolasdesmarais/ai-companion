"use client";

import { useVideoModal } from "@/hooks/use-video-modal";
import { X } from "lucide-react";
import { useEffect, useRef, useState } from "react";
import Draggable from "react-draggable";

export const VideoPreviewModal = () => {
  const videoModal = useVideoModal();
  const [isMounted, setIsMounted] = useState(false);
  const ref = useRef<HTMLVideoElement>(null);

  useEffect(() => {
    setIsMounted(true);
  }, []);

  useEffect(() => {
    ref.current?.load();
    ref.current?.addEventListener("ended", () => {
      if (ref.current) {
        ref.current.currentTime = 0;
      }
    });
  }, [videoModal.src]);

  useEffect(() => {
    if (ref.current?.ended) {
    }
  }, [ref.current?.ended]);

  if (!isMounted || !videoModal.isOpen) {
    return null;
  }

  return (
    <Draggable handle=".handle">
      <div className="fixed top-8 right-8 w-64 handle">
        <X
          className="absolute top-2 right-2 h-6 w-6 cursor-pointer hover:text-ring z-20"
          onClick={videoModal.onClose}
        />
        <video
          ref={ref}
          src={videoModal.src}
          autoPlay
          playsInline
          onClick={() => ref.current?.play()}
          className="rounded-lg object-cover"
        >
          <p>
            Your browser doesn&apos;t support HTML video. Here is a
            <a href={videoModal.src}>link to the video</a> instead.
          </p>
        </video>
      </div>
    </Draggable>
  );
};
