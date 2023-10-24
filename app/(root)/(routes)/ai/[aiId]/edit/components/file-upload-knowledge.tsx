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
import { Prisma } from "@prisma/client";
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
  const [removing, setRemoving] = useState("");
  const inputFileRef = useRef<HTMLInputElement>(null);
  const { toast } = useToast();

  const isLoading = form.formState.isSubmitting;

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
      const response = await axios.post(
        `/api/knowledge?filename=${encodeURIComponent(
          file.name
        )}&type=${encodeURIComponent(file.type)}`,
        data
      );
      const current = form.getValues("knowledge");
      form.setValue(
        "knowledge",
        [
          ...current,
          { knowledge: response.data, knowledgeId: response.data.id },
        ],
        { shouldDirty: true }
      );
      inputFileRef.current.value = "";
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

  const removeKnowledge = async (id: string) => {
    setRemoving(id);
    const aiId = form.getValues("id");
    try {
      await axios.delete(`/api/knowledge/${id}/${aiId}`);

      const current = form.getValues("knowledge");
      form.setValue(
        "knowledge",
        current.filter((i: any) => i.knowledge.id !== id),
        { shouldDirty: false }
      );
    } catch (error: any) {
      toast({
        variant: "destructive",
        description:
          String((error as AxiosError).response?.data) ||
          "Something went wrong.",
        duration: 6000,
      });
    }
    setRemoving("");
  };

  return (
    <>
      <FormField
        name="knowledge"
        control={form.control}
        render={({ field }) => (
          <FormItem>
            <FormLabel>Upload your file</FormLabel>
            <div>
              {field.value.map((item: any) => (
                <div
                  key={item.knowledgeId}
                  className="flex items-center justify-between my-2"
                >
                  <p className="text-sm px-3 py-2 bg-background rounded-lg  w-full ">
                    {item.knowledge.name}
                  </p>
                  <Button
                    type="button"
                    variant="outline"
                    onClick={() => removeKnowledge(item.knowledgeId)}
                  >
                    {removing === item.knowledgeId ? (
                      <Loader className="w-4 h-4 spinner" />
                    ) : (
                      <Trash2 className="w-4 h-4" />
                    )}
                  </Button>
                </div>
              ))}
            </div>
            <div>
              <div className="flex my-2">
                <Input name="file" ref={inputFileRef} type="file" />
                <Button
                  type="button"
                  disabled={isLoading || uploading}
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
        )}
      />
      <div className="flex justify-between w-full flex-row-reverse">
        <Button onClick={goBack} variant="link">
          Back
        </Button>
      </div>
    </>
  );
};
