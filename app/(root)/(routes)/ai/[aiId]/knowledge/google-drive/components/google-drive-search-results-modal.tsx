import { Button } from "@/components/ui/button";
import {
  Dialog,
  DialogContent,
  DialogFooter,
  DialogHeader,
  DialogTitle,
} from "@/components/ui/dialog";
import { Separator } from "@/components/ui/separator";
import { GoogleDriveFile } from "@/domain/types/GoogleDriveSearchResponse";

interface GoogleDriveSearchResultsModalProps {
  isVisible: boolean;
  onClose: () => void;
  results: GoogleDriveFile[];
}

export const GoogleDriveSearchResultsModal: React.FC<
  GoogleDriveSearchResultsModalProps
> = ({ isVisible, onClose, results }) => {
  return (
    <Dialog open={isVisible} onOpenChange={onClose}>
      <DialogContent>
        <DialogHeader className="space-y-4">
          <DialogTitle className="text-center">Search Results</DialogTitle>
        </DialogHeader>
        <Separator />
        <div className="space-y-8">
          {/* Table */}
          <table className="min-w-full table-auto">
            <thead>
              <tr>
                <th className="px-4 py-2">Name</th>
                <th className="px-4 py-2">Type</th>
              </tr>
            </thead>
            <tbody>
              {results &&
                results.map((file) => (
                  <tr key={file.id}>
                    <td className="border px-4 py-2">{file.name}</td>
                    <td className="border px-4 py-2">{file.type}</td>
                  </tr>
                ))}
            </tbody>
          </table>
        </div>
        <DialogFooter>
          <div className="flex justify-end w-full">
            <Button size="lg" variant="ring" onClick={onClose}>
              Close
            </Button>
          </div>
        </DialogFooter>
      </DialogContent>
    </Dialog>
  );
};
