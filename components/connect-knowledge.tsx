"use client";
import { Button } from "@/components/ui/button";
import {
  FormDescription,
  FormItem,
  FormLabel,
  FormMessage,
} from "@/components/ui/form";
import { useToast } from "@/components/ui/use-toast";
import { Loader, Unplug } from "lucide-react";
import { useState } from "react";

interface Props {
  goBack: () => void;
  form: any;
}

export const ConnectKnowledge = ({ goBack, form }: Props) => {
  const [uploading, setUploading] = useState(false);
  const { toast } = useToast();

  const aiId = form.getValues("id");

  return (
    <div className="w-full p-6 bg-accent/30">
      <FormItem>
        <FormLabel>Connect Knowledge</FormLabel>
        <FormDescription>
          Link existing knowledge you created for another AI.
        </FormDescription>
        <FormMessage />
      </FormItem>
      <div className="flex flex-row-reverse w-full mt-4">
        <Button
          type="button"
          disabled={uploading}
          onClick={() => {}}
          variant="ring"
        >
          Connect
          {uploading ? (
            <Loader className="w-4 h-4 ml-2 spinner" />
          ) : (
            <Unplug className="w-4 h-4 ml-2" />
          )}
        </Button>
      </div>
    </div>
  );
};
