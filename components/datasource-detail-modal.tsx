"use client";

import {
  Dialog,
  DialogContent,
  DialogHeader,
  DialogTitle,
} from "@/components/ui/dialog";
import { Separator } from "@/components/ui/separator";

type Props = {
  dataSource: any;
  onClose: () => void;
};

export const DataSourceDetailModal = ({ dataSource, onClose }: Props) => {
  if (!dataSource) return null;
  console.log(dataSource);
  return (
    <Dialog
      open={!!dataSource}
      onOpenChange={(isOpen) => {
        if (!isOpen) {
          onClose();
        }
      }}
    >
      <DialogContent>
        <DialogHeader className="space-y-4">
          <DialogTitle className="text-center">
            {dataSource.name} Details
          </DialogTitle>
        </DialogHeader>
        <Separator />
        {dataSource.knowledges.map(({ knowledge }: any) => (
          <div key={knowledge.id} className="p-2">
            <div>{knowledge.name}</div>
            <div>{knowledge.indexStatus}</div>
          </div>
        ))}
      </DialogContent>
    </Dialog>
  );
};
