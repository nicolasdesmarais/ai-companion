"use client";
import { Button } from "@/components/ui/button";
import {
  FormDescription,
  FormItem,
  FormLabel,
  FormMessage,
} from "@/components/ui/form";
import { Input } from "@/components/ui/input";
import { useToast } from "@/components/ui/use-toast";
import { AxiosError } from "axios";
import { FileText, Loader } from "lucide-react";
import { useRef, useState } from "react";
import { knowledgeTypes } from "./knowledge-types";
import { upload } from "@vercel/blob/client";
import { delay } from "@/src/lib/utils";

interface FileUploadKnowledgeProps {
  goBack: () => void;
  form: any;
}

export const FileUploadKnowledge = ({
  goBack,
  form,
}: FileUploadKnowledgeProps) => {
  const [uploading, setUploading] = useState(false);
  const inputFileRef = useRef<HTMLInputElement>(null);
  const { toast } = useToast();

  const aiId = form.getValues("id");

  const uploadDocument = async () => {
    setUploading(true);
    if (
      !inputFileRef.current?.files ||
      inputFileRef.current?.files.length === 0
    ) {
      toast({
        variant: "destructive",
        description: "No file selected.",
        duration: 6000,
      });
      setUploading(false);
      return;
    }
    const file = inputFileRef.current.files[0];
    const fileType = file.type || "text/plain";

    if (
      !fileType.startsWith("text/") &&
      knowledgeTypes.findIndex((format) => format.type === fileType) === -1
    ) {
      toast({
        variant: "destructive",
        description: `This file format is not supported: ${fileType}.`,
        duration: 6000,
      });
      setUploading(false);
      return;
    }
    try {
      await upload(file.name, file, {
        access: "public",
        handleUploadUrl: `/api/v1/ai/${aiId}/data-sources/file-blob`,
      });
      await delay(1000);
      inputFileRef.current.value = "";
      toast({ description: "File uploaded." });
      goBack();
    } catch (error: any) {
      console.error(error);
      toast({
        variant: "destructive",
        description: "Something went wrong.",
        duration: 6000,
      });
    }
    setUploading(false);
  };

  return (
    <div className="w-full p-6 bg-accent/30">
      <FormItem>
        <FormLabel>Upload your file</FormLabel>
        <div>
          <div className="flex my-2">
            <Input name="file" ref={inputFileRef} type="file" />
          </div>
        </div>
        <FormDescription>
          Add custom knowledge to your AI. Max file size: 500Mb. <br />
          The following formats are supported:{" "}
          {knowledgeTypes
            .map((format) => (format.type !== "URL" ? format.name : null))
            .join(", ")}
        </FormDescription>
        <FormMessage />
      </FormItem>
      <div className="flex flex-row-reverse w-full mt-4">
        <Button
          type="button"
          disabled={uploading}
          onClick={() => uploadDocument()}
          variant="ring"
        >
          Upload
          {uploading ? (
            <Loader className="w-4 h-4 ml-2 spinner" />
          ) : (
            <FileText className="w-4 h-4 ml-2" />
          )}
        </Button>
      </div>
    </div>
  );
};
