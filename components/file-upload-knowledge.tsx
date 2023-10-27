"use client";
import { Button } from "@/components/ui/button";
import {
  FormDescription,
  FormItem,
  FormLabel,
  FormMessage,
} from "@/components/ui/form";
import { useRef, useState } from "react";
import { useToast } from "@/components/ui/use-toast";
import axios, { AxiosError } from "axios";
import { FileText, Loader } from "lucide-react";
import { Input } from "@/components/ui/input";
import { knowledgeTypes } from "./knowledge-types";

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

    if (
      knowledgeTypes.findIndex((format) => format.type === file.type) === -1
    ) {
      toast({
        variant: "destructive",
        description: "This file format is not supported",
        duration: 6000,
      });
    }
    try {
      const data = new FormData();
      data.set("file", file);
      await axios.post(
        `/api/v1/ai/${aiId}/knowledge/file?filename=${encodeURIComponent(
          file.name
        )}&type=${encodeURIComponent(file.type)}`,
        data
      );
      inputFileRef.current.value = "";
      toast({ description: "File uploaded." });
      goBack();
    } catch (error: any) {
      toast({
        variant: "destructive",
        description:
          String((error as AxiosError).response?.data) ||
          "Something went wrong.",
        duration: 6000,
      });
    }
    setUploading(false);
  };

  return (
    <div className="w-full p-6 bg-gray-900 text-white">
      <FormItem>
        <FormLabel>Upload your file</FormLabel>
        <div>
          <div className="flex my-2">
            <Input name="file" ref={inputFileRef} type="file" />
          </div>
        </div>
        <FormDescription>
          Add custom knowledge to your AI. Max file size: 4.5Mb. <br />
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
