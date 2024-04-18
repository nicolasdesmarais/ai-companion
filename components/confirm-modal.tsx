"use client";
import { Button } from "@/components/ui/button";
import {
  Dialog,
  DialogContent,
  DialogDescription,
  DialogFooter,
  DialogHeader,
  DialogTitle,
} from "@/components/ui/dialog";
import { Separator } from "@/components/ui/separator";
import { useConfirmModal } from "@/hooks/use-confirm-modal";
import { Loader } from "lucide-react";
import { useEffect, useState } from "react";

export const ConfirmModal = () => {
  const [isMounted, setIsMounted] = useState(false);
  const [loading, setLoading] = useState(false);
  const confirmModal = useConfirmModal();

  useEffect(() => {
    setIsMounted(true);
  }, []);

  if (!isMounted) {
    return null;
  }

  return (
    <Dialog
      open={confirmModal.isOpen}
      onOpenChange={() => {
        confirmModal.onClose();
      }}
    >
      <DialogContent>
        <DialogHeader className="space-y-4">
          <DialogTitle className="text-left">{confirmModal.title}</DialogTitle>
          <DialogDescription className="text-left space-y-2">
            {confirmModal.loading ? (
              <div className="flex justify-center items-center h-32">
                <Loader className="w-16 h-16 spinner" />
              </div>
            ) : (
              confirmModal.body
            )}
          </DialogDescription>
        </DialogHeader>
        <Separator />
        <DialogFooter>
          {confirmModal.footer ? (
            confirmModal.footer
          ) : (
            <div className="flex justify-between w-full">
              <Button
                size="lg"
                variant="destructive"
                onClick={() => {
                  confirmModal.onClose();
                  confirmModal.onConfirm();
                }}
                className="bg-red-600 hover:bg-red-700"
                disabled={loading}
                type="button"
              >
                Confirm
              </Button>
              <Button
                onClick={() => confirmModal.onClose()}
                variant="link"
                type="button"
              >
                Cancel
              </Button>
            </div>
          )}
        </DialogFooter>
      </DialogContent>
    </Dialog>
  );
};
