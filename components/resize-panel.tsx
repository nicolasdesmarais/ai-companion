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

// why cant web mouse events *just work*?
const INERTIA_MULTIPLIER = 1.9;

type Props = {
  children: ReactNode;
  name: string;
  className?: string;
  initial: number;
  max: number;
  min: number;
};

export const ResizePanel = ({
  children,
  className,
  name,
  initial,
  max,
  min,
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
        setSidebarWidth((width) =>
          Math.min(
            max,
            Math.max(
              min,
              Math.round(width + mouseMoveEvent.movementX * INERTIA_MULTIPLIER)
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
      className={cn(className, "hidden md:flex max-w-2xl min-w-[78px]")}
      ref={sidebarRef}
      style={{ width: initial }}
      onMouseDown={(e) => e.preventDefault()}
    >
      {children}
      <div
        className={cn(
          "hover:bg-ring w-1 cursor-col-resize",
          isResizing && "bg-ring"
        )}
        onMouseDown={startResizing}
      ></div>
    </div>
  );
};
