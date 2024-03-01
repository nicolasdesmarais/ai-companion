"use client";

import { Copy, Eye } from "lucide-react";
import { useTheme } from "next-themes";
import { BeatLoader } from "react-spinners";

import { BotAvatar } from "@/components/bot-avatar";
import { Button } from "@/components/ui/button";
import { useToast } from "@/components/ui/use-toast";
import { UserAvatar } from "@/components/user-avatar";
import { cn } from "@/src/lib/utils";
import { Role } from "@prisma/client";
import hljs from "highlight.js";
import "highlight.js/styles/github.css";
import { Marked } from "marked";
import { markedHighlight } from "marked-highlight";
import { useEffect, useState } from "react";

const delay = 15;

const marked = new Marked(
  markedHighlight({
    langPrefix: "hljs language-",
    highlight(code, lang) {
      const language = hljs.getLanguage(lang) ? lang : "plaintext";
      return hljs.highlight(code, { language }).value;
    },
  })
);
marked.use({
  extensions: [
    {
      name: "link",
      renderer: (token): string | false | undefined => {
        if (!token.text) {
          return;
        }
        if (!token.href) {
          return token.text;
        }
        return `<a href=${token.href} target="_blank">${token.text}</a>`;
      },
    },
  ],
});

export interface ChatMessageProps {
  role: Role;
  content?: string;
  isLoading?: boolean;
  src?: string;
  onInspect?: () => void;
  isAnimated?: boolean;
  scrollRef?: any;
}

export const ChatMessage = ({
  role,
  content,
  isLoading,
  src,
  onInspect,
  isAnimated,
  scrollRef,
}: ChatMessageProps) => {
  const [animatedText, setAnimatedText] = useState<string>("");
  const [animatedIndex, setAnimatedIndex] = useState<number>(0);
  const { toast } = useToast();
  const { theme } = useTheme();

  useEffect(() => {
    if (!content || !isAnimated) {
      return;
    }
    if (animatedIndex < content.length) {
      const timeout = setTimeout(() => {
        setAnimatedText((prevText) => prevText + content[animatedIndex]);
        setAnimatedIndex((prevIndex) => prevIndex + 1);
        scrollRef?.current?.scrollIntoView();
      }, delay);

      return () => clearTimeout(timeout);
    }
  }, [animatedIndex, delay, animatedText, content, isAnimated]);

  const onCopy = () => {
    if (!content) {
      return;
    }

    navigator.clipboard.writeText(content);
    toast({
      description: "Message copied to clipboard.",
      duration: 3000,
    });
  };

  const renderedContent = isAnimated
    ? animatedText + '<span class="blinking-cursor">|</span>'
    : content;

  return (
    <div
      className={cn(
        "group flex items-start gap-x-1 sm:gap-x-3 py-4 w-full",
        role === "user" && "justify-end"
      )}
    >
      {role !== "user" && src && <BotAvatar src={src} />}
      <div className="rounded-md px-2 sm:px-4 sm:py-2 max-w-xs sm:max-w-sm lg:max-w-lg xl:max-w-xl 2xl:max-w-2xl text-sm bg-primary/10 place-self-center">
        {isLoading ? (
          <BeatLoader color={theme === "light" ? "black" : "white"} size={5} />
        ) : (
          <div
            className="markdown-chat-message"
            dangerouslySetInnerHTML={{
              __html: renderedContent ? marked.parse(renderedContent) : "",
            }}
          ></div>
        )}
      </div>
      {role === "user" && <UserAvatar />}
      {role !== "user" && !isLoading && (
        <Button
          onClick={onCopy}
          className="opacity-0 group-hover:opacity-100 transition shrink-0"
          size="icon"
          variant="ghost"
        >
          <Copy className="w-4 h-4" />
        </Button>
      )}
      {onInspect && role !== "user" && !isLoading && (
        <Button
          onClick={onInspect}
          className="opacity-0 group-hover:opacity-100 transition hidden md:inline-flex"
          size="icon"
          variant="ghost"
        >
          <Eye className="w-4 h-4" />
        </Button>
      )}
    </div>
  );
};
