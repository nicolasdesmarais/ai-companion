"use client";
import { Table } from "@/components/table";
import { Button } from "@/components/ui/button";
import {
  FormControl,
  FormDescription,
  FormItem,
  FormLabel,
} from "@/components/ui/form";
import {
  Select,
  SelectContent,
  SelectItem,
  SelectTrigger,
  SelectValue,
} from "@/components/ui/select";
import { useToast } from "@/components/ui/use-toast";
import { EntityNotFoundError } from "@/src/domain/errors/Errors";
import { UserOAuthTokenEntity } from "@/src/domain/models/OAuthTokens";
import { DataSourceRefreshPeriod } from "@prisma/client";
import axios from "axios";
import { format } from "date-fns";
import { Loader, Server } from "lucide-react";
import { useEffect, useState } from "react";
import { getDataSourceRefreshPeriodLabel } from "./datasource-refresh-periods";
import {
  FileType,
  getLabelFromFileType,
} from "@/src/adapter-in/api/DataSourcesApi";
import mime from "mime-types";

const ADD_ACCOUNT_OPTION = "add-account";

interface Props {
  aiId: string;
  goBack: () => void;
}

export const OneDriveKnowledge = ({ aiId, goBack }: Props) => {
  const [loading, setLoading] = useState(false);
  const [searching, setSearching] = useState(true);
  const [accounts, setAccounts] = useState<UserOAuthTokenEntity[]>([]);
  const [selectedAccount, setSelectedAccount] = useState("");
  const [popupWindow, setPopupWindow] = useState<Window | null>(null);
  const [searchResults, setSearchResults] = useState<any[]>([]);
  const [selectedFile, setSelectedFile] = useState<any | null>(null);
  const [searchTerm, setSearchTerm] = useState("");
  const [uploading, setUploading] = useState(false);
  const [dataRefreshPeriod, setDataRefreshPeriod] =
    useState<DataSourceRefreshPeriod | null>(DataSourceRefreshPeriod.NEVER);
  const { toast } = useToast();

  useEffect(() => {
    fetchAccount();
  }, []);

  useEffect(() => {
    if (selectedAccount) {
      handleSearch();
    }
  }, [selectedAccount]);

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

  const fetchAccount = async () => {
    setLoading(true);
    try {
      const response = await axios.get(
        `/api/v1/integrations/onedrive/accounts`
      );
      setAccounts(response.data);
      if (response.data.length > 0) {
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

  const handleConnectClick = async () => {
    // Open a new popup window
    const width = 600;
    const height = 600;
    const left = window.innerWidth / 2 - width / 2;
    const top = window.innerHeight / 2 - height / 2;
    const authPopup = window.open(
      "/api/v1/integrations/onedrive/auth",
      "authPopup",
      `width=${width},height=${height},top=${top},left=${left}`
    );
    if (authPopup) {
      setPopupWindow(authPopup);
    }
  };

  const handleAccountChange = (value: string) => {
    setSelectedAccount(value);
    if (value === ADD_ACCOUNT_OPTION) {
      handleConnectClick();
    }
  };

  const handleSearch = async () => {
    setSearching(true);
    setSelectedFile(null);
    try {
      const response = await axios.post(
        `/api/v1/integrations/onedrive/search`,
        {
          oauthTokenId: selectedAccount ?? "",
          searchTerm,
        }
      );
      setSearchResults(response.data.value);
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

  const handleLoad = async () => {
    if (!selectedFile || !selectedAccount) {
      return;
    }
    setUploading(true);

    try {
      await axios.post(`/api/v1/ai/${aiId}/data-sources/onedrive`, {
        oauthTokenId: selectedAccount,
        fileId: selectedFile.id,
        filename: selectedFile.name,
        dataRefreshPeriod: dataRefreshPeriod ?? DataSourceRefreshPeriod.NEVER,
      });
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
    <div className="w-full p-6 bg-accent/30 mb-8">
      <div className="mb-4">
        <h2 className="text-xl font-bold">Microsoft OneDrive Integration</h2>
        <p className="text-gray-400 mb-4">
          Choose a file or folder from your OneDrive to train your AI.
        </p>
        {!loading ? (
          <>
            <FormItem>
              <FormLabel>Account</FormLabel>
              {accounts.length === 0 ? (
                <div>
                  <Button
                    onClick={handleConnectClick}
                    type="button"
                    variant="ring"
                  >
                    Connect Your Microsoft Account
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
                    <td className="p-2">
                      {file.folder
                        ? "Folder"
                        : getLabelFromFileType(
                            mime.lookup(file.name) as FileType
                          )}
                    </td>
                    <td className="p-2">{file.createdBy?.user?.displayName}</td>
                    <td className="p-2">
                      {format(
                        new Date(file.lastModifiedDateTime),
                        "h:mma M/d/yyyy "
                      )}
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
      {!loading && selectedAccount && (
        <div className="my-4">
          <FormItem>
            <FormLabel>Data Refresh Interval</FormLabel>
            <Select
              onValueChange={(value) =>
                setDataRefreshPeriod(value as DataSourceRefreshPeriod)
              }
              value={dataRefreshPeriod ?? ""}
            >
              <FormControl>
                <SelectTrigger className="bg-background">
                  <SelectValue>
                    {getDataSourceRefreshPeriodLabel(dataRefreshPeriod)}
                  </SelectValue>
                </SelectTrigger>
              </FormControl>
              <SelectContent>
                {Object.values(DataSourceRefreshPeriod).map((period) => (
                  <SelectItem key={period} value={period}>
                    {getDataSourceRefreshPeriodLabel(period)}
                  </SelectItem>
                ))}
              </SelectContent>
            </Select>
            <FormDescription>
              Determine how often your data source will be reindexed. Please be
              aware that this may increase costs.
            </FormDescription>
          </FormItem>
        </div>
      )}
      {selectedFile && (
        <>
          <div className="flex flex-row-reverse w-full mt-4">
            <Button
              type="button"
              onClick={handleLoad}
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
  );
};
