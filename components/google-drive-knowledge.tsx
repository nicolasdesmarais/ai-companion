"use client";
import { Table } from "@/components/table";
import { Button } from "@/components/ui/button";
import { FormControl, FormItem, FormLabel } from "@/components/ui/form";
import {
  Select,
  SelectContent,
  SelectItem,
  SelectTrigger,
  SelectValue,
} from "@/components/ui/select";
import { useToast } from "@/components/ui/use-toast";
import {
  CreateGoogleDriveKnowledgeRequest,
  GoogleDriveFile,
  GoogleDriveSearchRequest,
} from "@/src/adapter-in/api/GoogleDriveApi";
import { getLabelFromFileType } from "@/src/adapter-in/api/DataSourcesApi";
import { EntityNotFoundError } from "@/src/domain/errors/Errors";
import { UserOAuthTokenEntity } from "@/src/domain/models/OAuthTokens";
import { DataSourceRefreshPeriod } from "@prisma/client";
import axios from "axios";
import { format } from "date-fns";
import { Loader, Server } from "lucide-react";
import { useEffect, useState } from "react";
import { Banner } from "./ui/banner";
import { DataRefreshPeriod } from "./data-refresh-period";

const ADD_ACCOUNT_OPTION = "add-account";

interface FilesProps {
  aiId: string;
  goBack: () => void;
}

export const GoogleDriveForm = ({ aiId, goBack }: FilesProps) => {
  const [searchTerm, setSearchTerm] = useState("");
  const [popupWindow, setPopupWindow] = useState<Window | null>(null);
  const [searchResults, setSearchResults] = useState<GoogleDriveFile[]>([]);
  const [selectedFile, setSelectedFile] = useState<GoogleDriveFile | null>(
    null
  );
  const [selectedAccount, setSelectedAccount] = useState("");
  const [accounts, setAccounts] = useState<UserOAuthTokenEntity[]>([]);
  const [loading, setLoading] = useState(true);
  const [searching, setSearching] = useState(true);
  const [uploading, setUploading] = useState(false);
  const [setupRequired, setSetupRequired] = useState(false);
  const [dataRefreshPeriod, setDataRefreshPeriod] =
    useState<DataSourceRefreshPeriod | null>(DataSourceRefreshPeriod.NEVER);

  const fetchAccount = async () => {
    setLoading(true);
    try {
      const response = await axios.get(
        `/api/v1/integrations/google-drive/accounts`
      );
      setAccounts(response.data);
      if (response.data === null) {
        setSearching(false);
        setSetupRequired(true);
      } else if (response.data.length > 0) {
        setSelectedAccount(response.data[0]?.id);
      } else {
        setSearching(false);
      }
    } catch (error) {
      toast({
        variant: "destructive",
        description: "Something went wrong",
      });
    }

    setLoading(false);
  };

  useEffect(() => {
    fetchAccount();
  }, []);

  useEffect(() => {
    if (selectedAccount) {
      handleSearch();
    }
  }, [selectedAccount]);

  const { toast } = useToast();

  useEffect(() => {
    const popupInterval = setInterval(() => {
      if (popupWindow?.closed) {
        clearInterval(popupInterval);
        fetchAccount();
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
    setSearching(true);
    setSelectedFile(null);
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
    setSearching(false);
  };

  const handleContinue = async () => {
    if (!selectedFile || !selectedAccount) {
      return;
    }
    setUploading(true);

    try {
      const createKnowledgeRequest: CreateGoogleDriveKnowledgeRequest = {
        oauthTokenId: selectedAccount,
        fileId: selectedFile.id,
        filename: selectedFile.name,
        dataRefreshPeriod: dataRefreshPeriod ?? DataSourceRefreshPeriod.NEVER,
      };

      const response = await axios.post(
        `/api/v1/ai/${aiId}/data-sources/google-drive`,
        createKnowledgeRequest
      );
      goBack();
    } catch (error) {
      toast({
        variant: "destructive",
        description: "Something went wrong",
      });
    }
    setUploading(false);
  };

  return (
    <div>
      <div className="w-full p-6 bg-accent/30">
        <div className="mb-4">
          <h2 className="text-xl font-bold">Google Drive Integration</h2>
          <p className="text-gray-400 mb-4">
            Choose a file or folder from your Google Drive to train your AI.
          </p>

          {!loading ? (
            <>
              {setupRequired ? (
                <Banner className="my-2" variant="destructive">
                  Google Drive integration is not set up. Please go to Settings
                  to add the API keys or ask your company admin to set it up.
                </Banner>
              ) : (
                <FormItem>
                  <FormLabel>Account</FormLabel>
                  {accounts.length === 0 ? (
                    <div>
                      <Button
                        onClick={handleConnectClick}
                        type="button"
                        variant="ring"
                      >
                        Connect Your Google Account
                      </Button>
                    </div>
                  ) : null}
                  {accounts.length > 0 ? (
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
                  ) : null}
                </FormItem>
              )}
            </>
          ) : null}
        </div>
        {selectedAccount ? (
          <>
            <div className="mb-4">
              <div className="flex items-center">
                <input
                  className="border p-2 rounded w-full mr-2" // added mr-2 for spacing
                  type="text"
                  placeholder="Search term"
                  value={searchTerm}
                  onChange={(e) => setSearchTerm(e.target.value)}
                  onKeyDown={(e) => {
                    if (e.key === "Enter") {
                      e.preventDefault();
                      handleSearch();
                    }
                  }}
                />
                <Button onClick={handleSearch} variant="ring" type="button">
                  Search
                </Button>
              </div>
            </div>

            <div className="max-h-[32rem] overflow-auto border border-ring/30 rounded-sm">
              <Table
                headers={["NAME", "TYPE", "OWNER", "LAST MODIFIED"]}
                className="w-full max-h-60"
              >
                {!searching &&
                  searchResults &&
                  searchResults.map((file) => (
                    <tr
                      key={file.id}
                      className={file.id === selectedFile?.id ? "bg-ring" : ""}
                      onClick={() => setSelectedFile(file)}
                    >
                      <td className="p-2">{file.name}</td>
                      <td className="p-2">{getLabelFromFileType(file.type)}</td>
                      <td className="p-2">{file.owner}</td>
                      <td className="p-2">
                        {format(new Date(file.modifiedTime), "h:mma M/d/yyyy ")}
                      </td>
                    </tr>
                  ))}
              </Table>
              {!searching && searchResults && searchResults.length === 0 ? (
                <div className="flex items-center my-2 w-full">
                  <div className="mx-auto flex p-4 bg-background rounded-lg">
                    <Server className="w-6 h-6 mr-2" />
                    <p>No results found</p>
                  </div>
                </div>
              ) : null}
            </div>
          </>
        ) : null}
        {loading || searching ? (
          <div className="flex items-center my-2 w-full">
            <div className="mx-auto">
              <Loader className="w-8 h-8 spinner" />
            </div>
          </div>
        ) : null}
        {loading ||
          (!setupRequired && (
            <DataRefreshPeriod
              setDataRefreshPeriod={setDataRefreshPeriod}
              dataRefreshPeriod={dataRefreshPeriod}
            />
          ))}
        {selectedFile && (
          <>
            <div className="flex flex-row-reverse w-full mt-4">
              <Button
                type="button"
                onClick={handleContinue}
                disabled={!selectedFile || !selectedAccount}
                variant="ring"
              >
                Load
                {uploading ? (
                  <Loader className="w-4 h-4 ml-2 spinner" />
                ) : (
                  <Server className="w-4 h-4 ml-2" />
                )}
              </Button>
            </div>
          </>
        )}
      </div>
    </div>
  );
};
