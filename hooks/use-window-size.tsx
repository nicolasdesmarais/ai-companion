"use client";
import { useLayoutEffect, useState } from "react";

export type WindowSize = {
  width: number | null;
  height: number | null;
};

export function useWindowSize() {
  const [size, setSize] = useState<WindowSize>({
    width: null,
    height: null,
  });

  useLayoutEffect(() => {
    const handleResize = () => {
      setSize({
        width: window.innerWidth,
        height: window.innerHeight,
      });
    };

    handleResize();
    if (typeof window !== "undefined") {
      window.addEventListener("resize", handleResize);
    }

    return () => {
      if (typeof window !== "undefined") {
        window.removeEventListener("resize", handleResize);
      }
    };
  }, []);

  return size;
}

export default useWindowSize;
