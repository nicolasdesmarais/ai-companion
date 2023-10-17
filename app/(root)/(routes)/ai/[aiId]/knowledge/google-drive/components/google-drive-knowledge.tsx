"use client";
import { useState } from "react";

interface FilesProps {
  aiId: string;
}

export const GoogleDriveForm = ({ aiId }: FilesProps) => {
  const [folderName, setFolderName] = useState("");
  const [results, setResults] = useState<string[]>([]);

  const addKnowledge = async () => {
    const response = await fetch(`/api/v1/ai/${aiId}/knowledge/google-drive`, {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify({ folderName }),
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
          placeholder="Add Google Drive Folder"
          value={folderName}
          onChange={(e) => setFolderName(e.target.value)}
        />
        <button
          className="mt-2 p-2 bg-blue-500 text-white rounded"
          onClick={addKnowledge}
        >
          Add
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
