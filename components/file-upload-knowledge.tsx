"use client";
import { Button } from "@/components/ui/button";
import {
  FormDescription,
  FormField,
  FormItem,
  FormLabel,
  FormMessage,
} from "@/components/ui/form";
import { useRef, useState } from "react";
import { useToast } from "@/components/ui/use-toast";
import axios, { AxiosError } from "axios";
import { FileText, Loader, Trash2 } from "lucide-react";
import { Input } from "@/components/ui/input";

const supportedUploadFormats = [
  {
    name: "Text",
    type: "text/plain",
  },
  {
    name: "CSV",
    type: "text/csv",
  },
  {
    name: "PDF",
    type: "application/pdf",
  },
  {
    name: "DOCX",
    type: "application/vnd.openxmlformats-officedocument.wordprocessingml.document",
  },
  {
    name: "EPUB",
    type: "application/epub+zip",
  },
];

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
      supportedUploadFormats.findIndex(
        (format) => format.type === file.type
      ) === -1
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
    <FormItem>
      <FormLabel>Upload your file</FormLabel>
      <div>
        <div className="flex my-2">
          <Input name="file" ref={inputFileRef} type="file" />
          <Button
            type="button"
            disabled={uploading}
            variant="outline"
            onClick={() => uploadDocument()}
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
      <FormDescription>
        Add custom knowledge to your AI. Max file size: 4.5Mb. <br />
        The following formats are supported:{" "}
        {supportedUploadFormats.map((format) => format.name).join(", ")}
      </FormDescription>
      <FormMessage />
    </FormItem>
  );
};
