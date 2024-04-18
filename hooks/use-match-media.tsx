"use client";
import { useEffect, useMemo, useState } from "react";

const useMatchMedia = (mediaQuery: string): boolean => {
  const mediaQueryList = useMemo<MediaQueryList | null>(
    () =>
      typeof window === "undefined" ? null : window.matchMedia(mediaQuery),
    [mediaQuery]
  );
  const [matches, setMatches] = useState<boolean>(() =>
    mediaQueryList ? mediaQueryList.matches : false
  );

  useEffect(() => {
    if (mediaQueryList === null) return;
    setMatches(mediaQueryList.matches);
    const listener = (event_: MediaQueryListEventMap["change"]) =>
      setMatches(event_.matches);
    mediaQueryList.addEventListener("change", listener);
    return () => mediaQueryList.removeEventListener("change", listener);
  }, [mediaQueryList]);

  return matches;
};

export default useMatchMedia;
