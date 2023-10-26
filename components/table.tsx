"use client";

import { cn } from "@/src/lib/utils";
import { ReactNode } from "react";

type Props = {
  headers: any[];
  children: ReactNode;
  className?: string;
};
export const Table = ({ headers, children, className }: Props) => {
  return (
    <table className={cn("table-auto text-left", className)}>
      <thead className="border-y-2 p-2 border-ring">
        <tr>
          {headers.map((header) => (
            <th className="p-2" key={header}>
              {header}
            </th>
          ))}
        </tr>
      </thead>
      <tbody>{children}</tbody>
    </table>
  );
};
