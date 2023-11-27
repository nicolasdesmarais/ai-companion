"use client";
import { Button } from "@/components/ui/button";
import {
  Form,
  FormControl,
  FormField,
  FormItem,
  FormLabel,
  FormMessage,
} from "@/components/ui/form";
import { Textarea } from "@/components/ui/textarea";
import { ShareAIRequest } from "@/src/ports/api/ShareAIRequest";
import { zodResolver } from "@hookform/resolvers/zod";
import { AI, AIVisibility } from "@prisma/client";
import axios from "axios";
import { Copy } from "lucide-react";
import { useForm } from "react-hook-form";
import * as z from "zod";
import { useToast } from "./ui/use-toast";

const groupFormSchema = z.object({
  visibility: z.enum([
    AIVisibility.PRIVATE,
    AIVisibility.GROUP,
    AIVisibility.PUBLIC,
    AIVisibility.ORGANIZATION,
  ]),
  teammates: z.string(),
});

interface ShareAIFormProps {
  ai: AI;
  onSuccess: () => void;
}

export const ShareAIForm = ({ ai, onSuccess }: ShareAIFormProps) => {
  const { toast } = useToast();
  const aiLink = `appdirect.ai/ai/${ai.id}`;
  const form = useForm<z.infer<typeof groupFormSchema>>({
    resolver: zodResolver(groupFormSchema),
    defaultValues: {
      visibility: ai.visibility,
      teammates: "",
    },
  });

  const onSubmit = async (values: z.infer<typeof groupFormSchema>) => {
    try {
      const request: ShareAIRequest = {
        emails: values.teammates,
      };

      const response = await axios.put(`/api/v1/ai/${ai.id}/share`, request);
      if (response.status === 200) {
        onSuccess();
      } else {
        toast({
          description: `Something went wrong (${response.status})`,
          variant: "destructive",
        });
      }
    } catch (error) {
      toast({
        description: "Something went wrong",
        variant: "destructive",
      });
    }
  };
  const onCopy = () => {
    navigator.clipboard.writeText(aiLink);
    toast({
      description: "Link copied to clipboard.",
      duration: 3000,
    });
  };

  return (
    <div className="h-full p-4 space-y-2 max-w-3xl">
      <Form {...form}>
        <form onSubmit={form.handleSubmit(onSubmit)} className="space-y-8">
          <h3 className="text-lg font-medium">Share AI - {ai.name}</h3>
          {ai.visibility === AIVisibility.PUBLIC && (
            <div className="flex items-center">
              <p className="text-xs px-3 py-3 mr-2 bg-ring/10 rounded-lg w-full">
                {aiLink}
              </p>
              <Button
                onClick={onCopy}
                size="icon"
                variant="ghost"
                type="button"
              >
                <Copy className="w-4 h-4" />
              </Button>
            </div>
          )}
          <FormField
            name="teammates"
            control={form.control}
            render={({ field }) => (
              <FormItem>
                <FormLabel>Add teammates</FormLabel>
                <FormControl>
                  <Textarea
                    placeholder="Ex: jennifer.wallace@acme.com, joe.hamm@acme.com"
                    {...field}
                  />
                </FormControl>
                <FormMessage />
              </FormItem>
            )}
          />
          <div className="w-full flex flex-row-reverse">
            <Button size="lg" variant="ring">
              Share
            </Button>
          </div>
        </form>
      </Form>
    </div>
  );
};
