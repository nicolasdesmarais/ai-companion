import { Button } from "@/components/ui/button";
import {
  Dialog,
  DialogContent,
  DialogFooter,
  DialogHeader,
  DialogTitle,
} from "@/components/ui/dialog";
import { Separator } from "@/components/ui/separator";
import {
  GoogleDriveFile,
  getLabelFromFileType,
} from "@/domain/types/GoogleDriveSearchResponse";
import axios from "axios";
import React, { useEffect, useState } from "react";

interface GoogleDriveSearchResultsModalProps {
  isVisible: boolean;
  oauthTokenId: string;
  initialSearchTerm: string;
  results: GoogleDriveFile[];
  onClose: () => void;
  onSelect: (id: GoogleDriveFile | null) => void;
}

export const GoogleDriveSearchResultsModal: React.FC<
  GoogleDriveSearchResultsModalProps
> = ({
  isVisible,
  initialSearchTerm,
  oauthTokenId,
  results,
  onClose,
  onSelect,
}) => {
  const [searchTerm, setSearchTerm] = useState("");
  const [narrowedResults, setNarrowedResults] =
    useState<GoogleDriveFile[]>(results);
  const [selectedFile, setSelectedFile] = useState<GoogleDriveFile | null>(
    null
  );

  useEffect(() => {
    setNarrowedResults(results);
  }, [results]);

  const handleNarrowSearch = async () => {
    if (!searchTerm) {
      return;
    }

    const searchRequest: GoogleDriveSearchRequest = {
      oauthTokenId,
      searchTerms: [initialSearchTerm, searchTerm],
    };

    const response = await axios.post(
      `/api/v1/integrations/google-drive/search`,
      searchRequest
    );

    setNarrowedResults(response.data.files);
  };

  return (
    <Dialog open={isVisible} onOpenChange={onClose}>
      <DialogContent className="overflow-auto">
        <DialogHeader className="space-y-4">
          <DialogTitle className="text-center">Search Results</DialogTitle>
        </DialogHeader>
        <Separator />
        <div className="space-y-8">
          <div className="flex items-center space-x-4">
            <div>
              <h3>Narrow Search</h3>
              <input
                placeholder="search term"
                value={searchTerm}
                onChange={(e) => setSearchTerm(e.target.value)}
              />
            </div>
            <Button onClick={handleNarrowSearch}>Search</Button>
          </div>
          {/* Table */}
          <div className="overflow-x-auto">
            <table className="min-w-full table-auto">
              <thead>
                <tr>
                  <th className="px-4 py-2">NAME</th>
                  <th className="px-4 py-2">TYPE</th>
                  <th className="px-4 py-2">OWNER</th>
                  <th className="px-4 py-2">LAST MODIFIED</th>
                </tr>
              </thead>
              <tbody>
                {narrowedResults &&
                  narrowedResults.map((file) => (
                    <tr
                      key={file.id}
                      className={
                        file.id === selectedFile?.id ? "bg-gray-200" : ""
                      }
                      onClick={() => setSelectedFile(file)}
                    >
                      <td className="border px-4 py-2">{file.name}</td>
                      <td className="border px-4 py-2">
                        {getLabelFromFileType(file.type)}
                      </td>
                      <td className="border px-4 py-2">{file.owner}</td>
                      <td className="border px-4 py-2">{file.modifiedTime}</td>
                    </tr>
                  ))}
              </tbody>
            </table>
          </div>
        </div>
        <DialogFooter>
          <div className="flex justify-end w-full space-x-4">
            <Button onClick={() => onSelect(selectedFile)}>Select</Button>
            <Button size="lg" variant="ring" onClick={onClose}>
              Cancel
            </Button>
          </div>
        </DialogFooter>
      </DialogContent>
    </Dialog>
  );
};
