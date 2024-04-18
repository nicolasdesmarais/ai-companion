import { ModeToggle } from "@/components/mode-toggle";
import { AIDetailDto } from "@/src/domain/models/AI";
import { cn } from "@/src/lib/utils";
import { LogIn, MessageSquare, Plus, Store } from "lucide-react";
import Image from "next/image";
import Link from "next/link";

interface Props {
  className?: string;
  isChat?: boolean;
  ais: AIDetailDto[];
}

export const PublicSidebar = ({ className, ais, isChat = false }: Props) => {
  const itemClass =
    "text-muted-foreground text-xs group py-3 px-8 flex w-full justify-center font-medium cursor-pointer hover:text-primary hover:bg-primary/10 rounded-lg transition";
  return (
    <div
      className={cn(
        "p-3 flex-1 flex justify-between flex-col h-full overflow-y-auto overflow-x-hidden",
        className
      )}
    >
      <div className="space-y-2 flex flex-col items-center">
        <div className="w-16">
          <Link href="/signup">
            <Image
              src="/appdirect-blue-gradient.jpeg"
              alt="AppDirect Logo"
              width="64"
              height="64"
              className="rounded-lg"
            />
          </Link>
        </div>
        <div className={cn(itemClass, isChat && "bg-accent text-primary")}>
          <Link
            href={ais && ais.length ? `/public/ai/${ais[0].id}` : "/signup"}
          >
            <div className="flex flex-col items-center flex-1">
              <MessageSquare className="h-5 w-5 mb-1" />
              Chat
            </div>
          </Link>
        </div>
        <div className={cn(itemClass, !isChat && "bg-accent text-primary")}>
          <Link href="/public">
            <div className="flex flex-col items-center flex-1">
              <Store className="h-5 w-5 mb-1" />
              <span className="w-12 text-center">Browse</span>
            </div>
          </Link>
        </div>
        <div className={cn(itemClass)}>
          <Link href="/signup">
            <div className="flex flex-col items-center flex-1">
              <Plus className="h-5 w-5 mb-1" />
              <span className="w-12 text-center">Create</span>
            </div>
          </Link>
        </div>
      </div>
      <div className="space-y-2 flex flex-col items-center py-3 px-8">
        <ModeToggle />
        <Link
          href="/login"
          className="flex rounded-full w-8 h-8 bg-ring items-center justify-center"
        >
          <LogIn className="text-white w-4 h-4" />
        </Link>
      </div>
    </div>
  );
};
