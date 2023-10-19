"use client";
import { useToast } from "@/components/ui/use-toast";
import { UserOAuthTokenEntity } from "@/domain/entities/OAuthTokenEntity";
import { EntityNotFoundError } from "@/domain/errors/Errors";
import { GoogleDriveFile } from "@/domain/types/GoogleDriveSearchResponse";
import { LoadFolderResponse } from "@/domain/types/LoadFolderResponse";
import axios from "axios";
import { useEffect, useState } from "react";
import { GoogleDriveSearchResultsModal } from "./google-drive-search-results-modal";

const ADD_ACCOUNT_OPTION = "add-account";

interface FilesProps {
  aiId: string;
  oauthTokens: UserOAuthTokenEntity[];
}

export const GoogleDriveForm = ({
  aiId,
  oauthTokens: oauthTokens,
}: FilesProps) => {
  const [searchTerm, setSearchTerm] = useState("");
  const [folderData, setFolderData] = useState<LoadFolderResponse | null>(null);
  const [popupWindow, setPopupWindow] = useState<Window | null>(null);
  const [isResultsModalVisible, setResultsModalVisible] = useState(false);
  const [searchResults, setSearchResults] = useState<GoogleDriveFile[]>([]);
  const [selectedFile, setSelectedFile] = useState<GoogleDriveFile | null>(
    null
  );

  const hasOAuthToken = oauthTokens.length > 0;
  const [selectedAccount, setAccount] = useState(
    hasOAuthToken ? oauthTokens[0].id : ""
  );

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
    console.log("selected account" + value);
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

  const search = async () => {
    try {
      const searchRequest: GoogleDriveSearchRequest = {
        oauthTokenId: selectedAccount ?? "",
        searchTerms: [searchTerm],
      };

      const response = await axios.post(
        `/api/v1/integrations/google-drive/search`,
        searchRequest
      );

      const files: GoogleDriveFile[] = response.data.files;

      setSearchResults(files);
      setResultsModalVisible(true);
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

  const handleFileSelect = (fileId: string | null) => {
    if (!fileId) {
      return;
    }
  };

  const addKnowledge = async () => {
    try {
      const response = await fetch(
        `/api/v1/ai/${aiId}/knowledge/google-drive`,
        {
          method: "POST",
          headers: { "Content-Type": "application/json" },
          body: JSON.stringify({ folderName: searchTerm }),
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
          {oauthTokens.map((token: UserOAuthTokenEntity) => (
            <option key={token.id} value={token.id}>
              {token.email}
            </option>
          ))}
          <option value={ADD_ACCOUNT_OPTION}>+ Add Account</option>
        </select>
      </div>
      {hasOAuthToken && (
        <div className="mb-4">
          <h3>Search Term</h3>
          <div className="flex items-center">
            <input
              className="border p-2 rounded w-full mr-2" // added mr-2 for spacing
              type="text"
              placeholder="Search term"
              value={searchTerm}
              onChange={(e) => setSearchTerm(e.target.value)}
            />
            <button
              className="p-2 bg-blue-500 text-white rounded"
              onClick={search}
            >
              Search
            </button>
          </div>
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
      )}
      <GoogleDriveSearchResultsModal
        isVisible={isResultsModalVisible}
        oauthTokenId={selectedAccount ?? ""}
        initialSearchTerm={searchTerm}
        onClose={() => setResultsModalVisible(false)}
        onSelect={setSelectedFile}
        results={searchResults}
      />
    </div>
  );
};
