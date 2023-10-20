"use client";
import Link from "next/link";
import React from "react";

interface Props {
  title: string;
  description: string;
  href: string;
}

const DataSourceCard: React.FC<Props> = ({ title, description, href }) => {
  return (
    <div className="card">
      <h2>{title}</h2>
      <p>{description}</p>
      <Link href={href} className="btn">
        Select
      </Link>

      <style jsx>{`
        .card {
          padding: 20px;
          border: 1px solid #ccc;
          border-radius: 8px;
        }

        .btn {
          display: inline-block;
          margin-top: 20px;
          padding: 10px 20px;
          background-color: #333;
          color: #fff;
          text-decoration: none;
          border: none;
          border-radius: 4px;
          cursor: pointer;
        }

        .btn:hover {
          background-color: #555;
        }
      `}</style>
    </div>
  );
};

export default DataSourceCard;
