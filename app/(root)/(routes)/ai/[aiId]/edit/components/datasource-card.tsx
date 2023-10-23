"use client";
import { LucideIcon } from "lucide-react";
import Link from "next/link";
import React from "react";

interface Props {
  title: string;
  description: string;
  href: string;
  icon: LucideIcon;
}

const DataSourceCard: React.FC<Props> = ({
  title,
  description,
  href,
  icon,
}) => {
  const Icon = icon;
  return (
    <div className="p-8 border rounded-xl">
      <Icon className="w-16 h-16" />
      <h2>{title}</h2>
      <p>{description}</p>
      <Link href={href} className="btn">
        Select
      </Link>
    </div>
  );
};

export default DataSourceCard;
