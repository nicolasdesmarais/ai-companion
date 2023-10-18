"use client";
import { useToast } from "@/components/ui/use-toast";
import { EntityNotFoundError } from "@/domain/errors/Errors";
import { LoadFolderResponse } from "@/domain/types/LoadFolderResponse";
import { useEffect, useState } from "react";

interface FilesProps {
  aiId: string;
  oauthTokenEmails: string[];
}

export const GoogleDriveForm = ({ aiId, oauthTokenEmails }: FilesProps) => {
  const [folderName, setFolderName] = useState("");
  const [folderData, setFolderData] = useState<LoadFolderResponse | null>(null);
  const [popupWindow, setPopupWindow] = useState<Window | null>(null);
  const { toast } = useToast();
  const hasOAuthToken = oauthTokenEmails.length > 0;

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
    try {
      const response = await fetch(
        `/api/v1/ai/${aiId}/knowledge/google-drive`,
        {
          method: "POST",
          headers: { "Content-Type": "application/json" },
          body: JSON.stringify({ folderName }),
        }
      );

      const folderData = await response.json();
      console.log(folderData);
      setFolderData(folderData);
    } catch (error) {
      if (error instanceof EntityNotFoundError) {
        toast({
          variant: "destructive",
          description: "Folder not found.",
        });
      } else {
        toast({
          variant: "destructive",
          description: "Something went wrong",
        });
      }
    }
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

          <div>
            {folderData?.folders.map((folder) => (
              <div key={folder.id}>
                <h2>Folder: {folder.name}</h2>
                {folder.files && folder.files.length > 0 ? (
                  <ul>
                    {folder.files.map((file) => (
                      <li key={file.id}>
                        File: {file.name} (Type: {file.type})
                      </li>
                    ))}
                  </ul>
                ) : (
                  <p>No files in this folder.</p>
                )}
              </div>
            ))}
          </div>
        </div>
      ) : (
        <button
          onClick={handleConnectClick}
          className="p-2 bg-blue-500 text-white rounded"
        >
          Connect to Google Drive
        </button>
      )}
    </div>
  );
};
