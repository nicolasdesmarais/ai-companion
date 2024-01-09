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
import { EntityNotFoundError } from "@/src/domain/errors/Errors";
import { UserOAuthTokenEntity } from "@/src/domain/models/OAuthTokens";
import axios from "axios";
import { format } from "date-fns";
import { Loader, Server } from "lucide-react";
import { useEffect, useState } from "react";

const ADD_ACCOUNT_OPTION = "add-account";

interface Props {
  aiId: string;
  goBack: () => void;
}

export const OneDriveKnowledge = ({ aiId, goBack }: Props) => {
  const [loading, setLoading] = useState(false);
  const [accounts, setAccounts] = useState<UserOAuthTokenEntity[]>([]);
  const [selectedAccount, setSelectedAccount] = useState("");
  const [popupWindow, setPopupWindow] = useState<Window | null>(null);

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

  return (
    <div className="w-full p-6 bg-accent/30 mb-8">
      <div className="mb-4">
        <h2 className="text-xl font-bold">OneDrive Integration</h2>
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
    </div>
  );
};
