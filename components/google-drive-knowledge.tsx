"use client";
import { Button } from "@/components/ui/button";
import { useToast } from "@/components/ui/use-toast";
import { UserOAuthTokenEntity } from "@/src/domain/entities/OAuthTokenEntity";
import { EntityNotFoundError } from "@/src/domain/errors/Errors";
import { CreateGoogleDriveKnowledgeRequest } from "@/src/domain/types/CreateGoogleDriveKnowledgeRequest";
import { GoogleDriveFile } from "@/src/domain/types/GoogleDriveSearchResponse";
import axios from "axios";
import { useEffect, useState } from "react";
import { GoogleDriveSearchResultsModal } from "./google-drive-search-results-modal";
import { Server, Loader } from "lucide-react";
import {
  Select,
  SelectContent,
  SelectItem,
  SelectTrigger,
  SelectValue,
} from "@/components/ui/select";
import {
  FormControl,
  FormDescription,
  FormField,
  FormItem,
  FormLabel,
  FormMessage,
} from "@/components/ui/form";

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
  const [loading, setLoading] = useState(true);

  useEffect(() => {
    const fetchAccount = async () => {
      setLoading(true);
      const response = await axios.get(
        `/api/v1/integrations/google-drive/accounts`
      );
      setAccounts(response.data);
      if (response.data.length > 0) {
        setSelectedAccount(response.data[0]?.id);
      }
      setLoading(false);
    };
    fetchAccount();
  }, []);

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

  const handleAccountChange = (value: string) => {
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
    setLoading(true);

    try {
      const createKnowledgeRequest: CreateGoogleDriveKnowledgeRequest = {
        oauthTokenId: selectedAccount,
        fileId: selectedFile.id,
      };

      const response = await axios.post(
        `/api/v1/ai/${aiId}/knowledge/google-drive`,
        createKnowledgeRequest
      );
      setLoading(false);
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
        <h2 className="text-xl font-bold">Google Drive Integration</h2>
        <p className="text-gray-400 mb-4">
          Choose a file or folders from your Google Drive to train your AI.
        </p>
        {loading ? (
          <div className="flex items-center my-2 w-full">
            <div className="mx-auto">
              <Loader className="w-8 h-8 spinner" />
            </div>
          </div>
        ) : null}
        {!loading ? (
          <>
            <FormItem>
              <FormLabel>Account</FormLabel>
              <Select
                disabled={loading}
                onValueChange={handleAccountChange}
                value={selectedAccount}
              >
                <FormControl>
                  <SelectTrigger className="bg-background">
                    <SelectValue placeholder="Select an account" />
                  </SelectTrigger>
                </FormControl>
                <SelectContent>
                  {accounts.map((token: UserOAuthTokenEntity) => (
                    <SelectItem key={token.id} value={token.id as string}>
                      {token.email}
                    </SelectItem>
                  ))}
                  <SelectItem value={ADD_ACCOUNT_OPTION}>
                    + Add Account
                  </SelectItem>
                </SelectContent>
              </Select>
            </FormItem>
          </>
        ) : null}
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
          Load
          {loading ? (
            <Loader className="w-4 h-4 ml-2 spinner" />
          ) : (
            <Server className="w-4 h-4 ml-2" />
          )}
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
