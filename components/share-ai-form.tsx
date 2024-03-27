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
import { ShareAIRequest } from "@/src/adapter-in/api/AIApi";
import { AIDetailDto } from "@/src/domain/models/AI";
import { zodResolver } from "@hookform/resolvers/zod";
import { AIVisibility } from "@prisma/client";
import axios from "axios";
import { Copy } from "lucide-react";
import { useForm } from "react-hook-form";
import {
  EmailIcon,
  EmailShareButton,
  FacebookIcon,
  FacebookShareButton,
  FacebookShareCount,
  LinkedinIcon,
  LinkedinShareButton,
  PocketIcon,
  PocketShareButton,
  RedditIcon,
  RedditShareButton,
  RedditShareCount,
  TelegramIcon,
  TelegramShareButton,
  TwitterShareButton,
  WhatsappIcon,
  WhatsappShareButton,
  XIcon,
} from "react-share";
import * as z from "zod";
import { useToast } from "./ui/use-toast";

const groupFormSchema = z.object({
  visibility: z.enum([
    AIVisibility.PRIVATE,
    AIVisibility.GROUP,
    AIVisibility.PUBLIC,
    AIVisibility.ORGANIZATION,
    AIVisibility.UNLISTED,
  ]),
  teammates: z.string(),
});

interface ShareAIFormProps {
  ai: AIDetailDto;
  onSuccess: () => void;
}

export const ShareAIForm = ({ ai, onSuccess }: ShareAIFormProps) => {
  const { toast } = useToast();
  let host = "https://appdirect.ai";
  if (typeof window !== "undefined") {
    host = window.location.origin;
  }
  let aiLink: string;
  if (ai.visibility === AIVisibility.PUBLIC) {
    aiLink = `${host}/public/ai/${ai.id}`;
  } else {
    aiLink = `${host}/ai/${ai.id}`;
  }
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

  const shareTitle = `Check out this AI: ${ai.name}`;

  return (
    <div className="h-full p-4 space-y-2 max-w-3xl">
      <Form {...form}>
        <form onSubmit={form.handleSubmit(onSubmit)} className="space-y-8">
          <h3 className="text-lg font-medium">Share {ai.name}</h3>
          {(ai.visibility === AIVisibility.PUBLIC ||
            ai.visibility === AIVisibility.UNLISTED) && (
            <div>
              <div className="flex justify-between mb-2">
                <div className="mb-4">
                  <TwitterShareButton
                    title={shareTitle}
                    url={aiLink}
                    hashtags={["appdirectai"]}
                    related={["AppDirect"]}
                    windowWidth={1100}
                    windowHeight={700}
                  >
                    <XIcon size={32} round />
                  </TwitterShareButton>
                </div>
                <div className="mb-4">
                  <LinkedinShareButton url={aiLink}>
                    <LinkedinIcon size={32} round />
                  </LinkedinShareButton>
                </div>
                <div>
                  <FacebookShareButton url={aiLink}>
                    <FacebookIcon size={32} round />
                  </FacebookShareButton>
                  <div className="flex justify-center">
                    <FacebookShareCount url={aiLink}>
                      {(count) => count}
                    </FacebookShareCount>
                  </div>
                </div>
                <div className="mb-4">
                  <EmailShareButton
                    url={aiLink}
                    subject={shareTitle}
                    body="body"
                  >
                    <EmailIcon size={32} round />
                  </EmailShareButton>
                </div>
                <div className="mb-4">
                  <PocketShareButton url={aiLink} title={shareTitle}>
                    <PocketIcon size={32} round />
                  </PocketShareButton>
                </div>
                <div>
                  <RedditShareButton
                    url={aiLink}
                    title={shareTitle}
                    windowWidth={1100}
                    windowHeight={700}
                  >
                    <RedditIcon size={32} round />
                  </RedditShareButton>
                  <div className="flex justify-center">
                    <RedditShareCount url={aiLink} />
                  </div>
                </div>
                <div className="mb-4">
                  <TelegramShareButton url={aiLink} title={shareTitle}>
                    <TelegramIcon size={32} round />
                  </TelegramShareButton>
                </div>
                <div className="mb-4">
                  <WhatsappShareButton
                    url={aiLink}
                    title={shareTitle}
                    separator=":: "
                    windowWidth={1100}
                    windowHeight={700}
                  >
                    <WhatsappIcon size={32} round />
                  </WhatsappShareButton>
                </div>
              </div>
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
            </div>
          )}
          <>
            <FormField
              name="teammates"
              control={form.control}
              render={({ field }) => (
                <FormItem>
                  <FormLabel>Share with teammates</FormLabel>
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
          </>
        </form>
      </Form>
    </div>
  );
};
