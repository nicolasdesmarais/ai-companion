"use client";
import { useSearchParams } from "next/navigation";
export const DataStoresDetails = () => {
  const searchParams = useSearchParams();

  const focus = searchParams.get("focus");
  return (
    <div className="mt-2">
      <div>Details: {focus}</div>
    </div>
  );
};
