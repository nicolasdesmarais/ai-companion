"use client";
import { useState } from "react";

interface FilesProps {
  userId: string;
}

export const BrowseGoogleDrive = ({ userId }: FilesProps) => {
  const [searchTerm, setSearchTerm] = useState("");
  const [results, setResults] = useState<string[]>([]);

  const searchFiles = async () => {
    const response = await fetch("/api/v1/integrations/google-drive/search", {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify({ searchTerm }),
    });

    const fileNames = await response.json();
    setResults(fileNames);
  };

  return (
    <div className="w-full p-4">
      <div className="mb-4">
        <input
          className="border p-2 rounded w-full"
          type="text"
          placeholder="Search for files"
          value={searchTerm}
          onChange={(e) => setSearchTerm(e.target.value)}
        />
        <button
          className="mt-2 p-2 bg-blue-500 text-white rounded"
          onClick={searchFiles}
        >
          Search
        </button>
      </div>
      <ul>
        {results.map((file) => (
          <li key={file}>{file}</li>
        ))}
      </ul>
    </div>
  );
};
