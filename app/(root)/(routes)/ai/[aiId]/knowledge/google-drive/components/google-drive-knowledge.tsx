"use client";
import { useToast } from "@/components/ui/use-toast";
import { UserOAuthTokenEntity } from "@/domain/entities/OAuthTokenEntity";
import { EntityNotFoundError } from "@/domain/errors/Errors";
import { LoadFolderResponse } from "@/domain/types/LoadFolderResponse";
import { useEffect, useState } from "react";

const ADD_ACCOUNT_OPTION = "add-account";

interface FilesProps {
  aiId: string;
  oauthTokens: UserOAuthTokenEntity[];
}

export const GoogleDriveForm = ({
  aiId,
  oauthTokens: oauthTokenEmails,
}: FilesProps) => {
  const [folderName, setFolderName] = useState("");
  const [folderData, setFolderData] = useState<LoadFolderResponse | null>(null);
  const [popupWindow, setPopupWindow] = useState<Window | null>(null);

  const hasOAuthToken = oauthTokenEmails.length > 0;
  const initialAccount = hasOAuthToken ? oauthTokenEmails[0].email : "";
  const [selectedAccount, setAccount] = useState(initialAccount);

  const { toast } = useToast();

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

  const handleAccountChange = (event: React.ChangeEvent<HTMLSelectElement>) => {
    const value = event.target.value;
    setAccount(value);
    if (value === ADD_ACCOUNT_OPTION) {
      handleConnectClick();
    }
  };

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
    <div className="w-full p-6 bg-gray-900 text-white">
      <div className="mb-4">
        <h2 className="text-xl font-bold">Data Store Name</h2>
        <p className="text-gray-400">
          Name this data set so you can use it later for other AIs. Choose
          something descriptive
        </p>
        <input
          type="text"
          placeholder="My business website index"
          className="mt-2 w-full p-2 bg-gray-800 border rounded border-gray-700"
        />
      </div>
      <div className="mb-4">
        <h2 className="text-xl font-bold">Google Drive Integration</h2>
        <p className="text-gray-400">
          Choose a file or folders from your Google Drive to train your AI.
        </p>
        <select
          value={selectedAccount}
          onChange={handleAccountChange}
          className="mt-2 w-full p-2 bg-gray-800 border rounded border-gray-700 text-white"
        >
          <option value="" disabled>
            Select an account
          </option>
          {oauthTokenEmails.map((option) => (
            <option key={option.id} value={option.email}>
              {option.email}
            </option>
          ))}
          <option value={ADD_ACCOUNT_OPTION}>+ Add Account</option>
        </select>
      </div>
      {hasOAuthToken && (
        <div className="mb-4">
          <h3 className="text-lg font-semibold">Search Term</h3>
          <input
            className="mt-2 w-full p-2 bg-gray-800 border rounded border-gray-700"
            type="text"
            placeholder="Add Google Drive Folder"
            value={folderName}
            onChange={(e) => setFolderName(e.target.value)}
          />
          <button
            className="mt-4 block w-full p-2 bg-blue-500 rounded hover:bg-blue-600 focus:outline-none focus:ring-2 focus:ring-blue-500"
            onClick={addKnowledge}
          >
            Search
          </button>

          <div className="mt-4">
            {folderData?.folders.map((folder) => (
              <div key={folder.id} className="mb-4">
                <h2 className="text-lg font-semibold">Folder: {folder.name}</h2>
                {folder.files && folder.files.length > 0 ? (
                  <ul className="list-disc pl-5">
                    {folder.files.map((file) => (
                      <li key={file.id}>
                        File: {file.name} (Type: {file.type})
                      </li>
                    ))}
                  </ul>
                ) : (
                  <p className="text-gray-400">No files in this folder.</p>
                )}
              </div>
            ))}
          </div>
        </div>
      )}
    </div>
  );
};
