"use client";
import useLocalStorageState from "@/hooks/use-local-storage-state";
import { cn } from "@/src/lib/utils";
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
};

export const ResizePanel = ({
  children,
  className,
  name,
  initial,
  max,
  min,
  position = "left",
}: Props) => {
  const sidebarRef = useRef(null);
  const [isResizing, setIsResizing] = useState(false);
  const [sidebarWidth, setSidebarWidth] = useLocalStorageState<number>(
    name,
    initial
  );

  const startResizing = useCallback((e: any) => {
    setIsResizing(true);
  }, []);

  const stopResizing = useCallback((e: any) => {
    setIsResizing(false);
  }, []);

  const resize = useCallback(
    (mouseMoveEvent: any) => {
      if (isResizing) {
        const movement = mouseMoveEvent.movementX;
        setSidebarWidth((width) =>
          Math.min(
            max,
            Math.max(
              min,
              Math.round(
                position === "right" ? width - movement : width + movement
              )
            )
          )
        );
      }
    },
    [isResizing, setSidebarWidth, max]
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
      (sidebarRef.current as any).style.width = sidebarWidth + "px";
    }
  }, [sidebarWidth]);

  return (
    <div
      className={cn(className, "shrink-0")}
      ref={sidebarRef}
      style={{ width: initial }}
      onMouseDown={(e) => e.preventDefault()}
    >
      {position === "left" && children}
      <div
        className={cn(
          "hover:bg-ring w-1 cursor-col-resize shrink-0",
          isResizing && "bg-ring"
        )}
        onMouseDown={startResizing}
      ></div>
      {position === "right" && children}
    </div>
  );
};
