"use client";

import chatService from "@/src/domain/services/ChatService";
import { getUserAuthorizationContext } from "@/src/security/utils/clientSecurityUtils";

import { redirectToSignIn } from "@clerk/nextjs";
import { useRouter } from "next/navigation";
import { useEffect } from "react";

interface ChatIdPageProps {
  params: {
    aiId: string;
  };
}
const ChatIdPage = ({ params }: ChatIdPageProps) => {
  const router = useRouter();
  const authorizationContext = getUserAuthorizationContext();

  useEffect(() => {
    const fetchData = async () => {
      if (!authorizationContext) {
        redirectToSignIn();
        return;
      }

      const aiChats = await chatService.getAIChats(
        authorizationContext,
        params.aiId
      );
      if (aiChats.data.length === 0) {
        try {
          const chat = await chatService.createChat(
            authorizationContext,
            params.aiId
          );
          router.push(`/chat/${chat.id}?new=true`);
        } catch (e) {
          router.push("/");
        }
      } else {
        router.push(`/chat/${aiChats.data[0].id}`);
      }
    };

    fetchData();
  }, [authorizationContext, params.aiId, router]);

  return null;
};

export default ChatIdPage;
