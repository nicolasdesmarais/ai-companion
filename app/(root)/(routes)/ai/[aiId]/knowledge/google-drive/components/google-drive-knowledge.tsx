"use client";

import { useEffect, useState } from "react";

interface FilesProps {
  aiId: string;
  hasOAuthToken: boolean;
}

export const GoogleDriveForm = ({ aiId, hasOAuthToken }: FilesProps) => {
  const [folderName, setFolderName] = useState("");
  const [results, setResults] = useState<string[]>([]);
  const [popupWindow, setPopupWindow] = useState<Window | null>(null);

  useEffect(() => {
    // Detect when the OAuth flow has completed (for instance, when the popup window is closed)
    const popupInterval = setInterval(() => {
      if (popupWindow?.closed) {
        clearInterval(popupInterval);

        // If needed, handle what should be done after the OAuth flow here.
        // For example, check if the user is authenticated or refresh some data.
      }
    }, 1000);

    // Cleanup interval on unmount
    return () => {
      clearInterval(popupInterval);
    };
  }, [popupWindow]);

  const handleConnectClick = () => {
    // Open a new popup window
    const width = 600;
    const height = 600;
    const left = window.innerWidth / 2 - width / 2;
    const top = window.innerHeight / 2 - height / 2;
    const authPopup = window.open(
      "/api/v1/integrations/google-drive/auth",
      "authPopup",
      `width=${width},height=${height},top=${top},left=${left}`
    );
    if (authPopup) {
      setPopupWindow(authPopup);
    }
  };

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
      {hasOAuthToken ? (
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
      ) : (
        <button
          onClick={handleConnectClick}
          className="p-2 bg-blue-500 text-white rounded"
        >
          Connect to Google Drive
        </button>
      )}

      <ul>
        {results.map((file) => (
          <li key={file}>{file}</li>
        ))}
      </ul>
    </div>
  );
};
