"use client";
import useLocalStorageState from "@/hooks/use-local-storage-state";
import { cn } from "@/src/lib/utils";
import { is } from "date-fns/locale";
import {
  ReactNode,
  useCallback,
  useRef,
  useState,
  useEffect,
  use,
} from "react";

type Props = {
  children: ReactNode;
  name: string;
  className?: string;
  initial: number;
  max: number;
  min: number;
  position?: "left" | "right";
  persist?: boolean;
  animationClassName?: string;
};

export const ResizePanel = ({
  children,
  className,
  name,
  initial,
  max,
  min,
  position = "left",
  persist = true,
  animationClassName,
}: Props) => {
  const sidebarRef = useRef(null);
  const [isResizing, setIsResizing] = useState(false);
  const [sidebarWidth, setSidebarWidth] = useLocalStorageState<number>(
    name,
    initial
  );
  const [ephemeralWidth, setEphemeralWidth] = useState(initial);
  const [allowAnimation, setAllowAnimation] = useState(true);

  const startResizing = useCallback((e: any) => {
    setAllowAnimation(false);
    setIsResizing(true);
  }, []);

  const stopResizing = useCallback((e: any) => {
    setIsResizing(false);
  }, []);

  const resize = useCallback(
    (mouseMoveEvent: any) => {
      if (isResizing) {
        const movement = mouseMoveEvent.movementX;
        const setter = (width: number) =>
          Math.min(
            max,
            Math.max(
              min,
              Math.round(
                position === "right" ? width - movement : width + movement
              )
            )
          );
        if (persist) {
          setSidebarWidth(setter);
        } else {
          setEphemeralWidth(setter);
        }
      }
    },
    [isResizing, setSidebarWidth, max, min, persist, position]
  );

  useEffect(() => {
    window.addEventListener("mousemove", resize);
    window.addEventListener("mouseup", stopResizing);
    return () => {
      window.removeEventListener("mousemove", resize);
      window.removeEventListener("mouseup", stopResizing);
    };
  }, [resize, stopResizing]);

  useEffect(() => {
    if (sidebarRef.current) {
      const w = persist ? sidebarWidth : ephemeralWidth;
      (sidebarRef.current as any).style.width = w + "px";
    }
  }, [sidebarWidth, ephemeralWidth, persist]);

  return (
    <div
      className={cn(
        className,
        "shrink-0",
        allowAnimation && animationClassName
      )}
      ref={sidebarRef}
      style={{ width: initial }}
      onMouseDown={(e) => e.preventDefault()}
    >
      {position === "left" && children}
      <div
        className={cn(
          "hover:bg-ring w-1 cursor-col-resize shrink-0 transition",
          isResizing && "bg-ring"
        )}
        onMouseDown={startResizing}
      ></div>
      {position === "right" && children}
    </div>
  );
};
