"use client";
import { cn } from "@/src/lib/utils";
import { ReactNode, useCallback, useRef, useState, useEffect } from "react";

type Props = {
  children: ReactNode;
  className?: string;
};

export const ResizePanel = ({ children, className }: Props) => {
  const sidebarRef = useRef(null);
  const [isResizing, setIsResizing] = useState(false);
  const [sidebarWidth, setSidebarWidth] = useState(360);

  const startResizing = useCallback((e: any) => {
    setIsResizing(true);
  }, []);

  const stopResizing = useCallback((e: any) => {
    setIsResizing(false);
  }, []);

  const resize = useCallback(
    (mouseMoveEvent: any) => {
      if (isResizing) {
        setSidebarWidth((width) => width + mouseMoveEvent.movementX * 1.9);
      }
    },
    [isResizing]
  );

  useEffect(() => {
    window.addEventListener("mousemove", resize);
    window.addEventListener("mouseup", stopResizing);
    return () => {
      window.removeEventListener("mousemove", resize);
      window.removeEventListener("mouseup", stopResizing);
    };
  }, [resize, stopResizing]);

  return (
    <div
      className={cn(className, "hidden md:flex max-w-2xl min-w-[78px]")}
      ref={sidebarRef}
      style={{ width: sidebarWidth }}
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
