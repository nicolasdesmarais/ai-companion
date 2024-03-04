"use client";

import { Button } from "@/components/ui/button";
import { Textarea } from "@/components/ui/textarea";
import { SendHorizonal } from "lucide-react";
import Link from "next/link";
import { useRouter } from "next/navigation";

interface Props {
  isLoading: boolean;
}

export const PublicChatForm = ({ isLoading }: Props) => {
  const router = useRouter();
  return (
    <div className="border-t border-primary/10 py-4 flex items-center gap-x-2 pl-4">
      <Textarea
        disabled={isLoading}
        onFocus={() => router.push("/sign-up")}
        placeholder="Type a message"
        className="rounded-lg bg-primary/10 max-h-80 h-[38px] min-h-[38px]"
      />
      <Link href="/sign-up">
        <Button disabled={isLoading} variant="ghost">
          <SendHorizonal className="w-6 h-6" />
        </Button>
      </Link>
    </div>
  );
};
