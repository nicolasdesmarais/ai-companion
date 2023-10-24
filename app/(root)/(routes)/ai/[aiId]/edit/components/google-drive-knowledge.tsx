"use client";
import { Button } from "@/components/ui/button";
import { useToast } from "@/components/ui/use-toast";
import { UserOAuthTokenEntity } from "@/domain/entities/OAuthTokenEntity";
import { EntityNotFoundError } from "@/domain/errors/Errors";
import { CreateGoogleDriveKnowledgeRequest } from "@/domain/types/CreateGoogleDriveKnowledgeRequest";
import { GoogleDriveFile } from "@/domain/types/GoogleDriveSearchResponse";
import axios from "axios";
import { redirect } from "next/navigation";
import { useEffect, useState } from "react";
import { GoogleDriveSearchResultsModal } from "./google-drive-search-results-modal";

const ADD_ACCOUNT_OPTION = "add-account";

interface FilesProps {
  aiId: string;
  goBack: () => void;
}

export const GoogleDriveForm = ({ aiId, goBack }: FilesProps) => {
  const [searchTerm, setSearchTerm] = useState("");
  const [popupWindow, setPopupWindow] = useState<Window | null>(null);
  const [isResultsModalVisible, setResultsModalVisible] = useState(false);
  const [searchResults, setSearchResults] = useState<GoogleDriveFile[]>([]);
  const [selectedFile, setSelectedFile] = useState<GoogleDriveFile | null>(
    null
  );
  const [selectedAccount, setSelectedAccount] = useState("");
  const [accounts, setAccounts] = useState<UserOAuthTokenEntity[]>([]);

  useEffect(() => {
    const fetchAccount = async () => {
      const response = await axios.get(
        `/api/v1/integrations/google-drive/accounts`
      );
      setAccounts(response.data);
      if (response.data.length > 0) {
        setSelectedAccount(response.data[0]?.id);
      }
    };
    fetchAccount();
  });

  const { toast } = useToast();

  useEffect(() => {
    const popupInterval = setInterval(() => {
      if (popupWindow?.closed) {
        clearInterval(popupInterval);
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
    setSelectedAccount(value);
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

  const handleSearch = async () => {
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

  const handleSelectFile = (file: GoogleDriveFile | null) => {
    if (!file) {
      return;
    }

    setSelectedFile(file);
    setResultsModalVisible(false);
  };

  const handleContinue = async () => {
    if (!selectedFile || !selectedAccount) {
      return;
    }

    try {
      const createKnowledgeRequest: CreateGoogleDriveKnowledgeRequest = {
        oauthTokenId: selectedAccount,
        fileId: selectedFile.id,
      };

      const response = await axios.post(
        `/api/v1/ai/${aiId}/knowledge/google-drive`,
        createKnowledgeRequest
      );
      goBack();
    } catch (error) {
      toast({
        variant: "destructive",
        description: "Something went wrong",
      });
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
          {accounts.map((token: UserOAuthTokenEntity) => (
            <option key={token.id} value={token.id}>
              {token.email}
            </option>
          ))}
          <option value={ADD_ACCOUNT_OPTION}>+ Add Account</option>
        </select>
      </div>
      {accounts.length ? (
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
            <Button onClick={handleSearch} variant="ring" type="button">
              Search
            </Button>
          </div>
        </div>
      ) : null}
      {selectedFile && (
        <div className="selected-file-section">
          <span>{selectedFile.name}</span>
          <Button onClick={() => setSelectedFile(null)} type="button">
            X
          </Button>
        </div>
      )}
      <div className="flex justify-between w-full">
        <Button
          type="button"
          onClick={handleContinue}
          disabled={!selectedFile || !selectedAccount}
          variant="ring"
        >
          Continue
        </Button>
        <Button onClick={goBack} variant="link" type="button">
          Back
        </Button>
      </div>

      <GoogleDriveSearchResultsModal
        isVisible={isResultsModalVisible}
        oauthTokenId={selectedAccount ?? ""}
        initialSearchTerm={searchTerm}
        onClose={() => setResultsModalVisible(false)}
        onSelect={handleSelectFile}
        results={searchResults}
      />
    </div>
  );
};
