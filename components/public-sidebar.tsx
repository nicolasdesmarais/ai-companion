import { ModeToggle } from "@/components/mode-toggle";
import { cn } from "@/src/lib/utils";
import { Permission } from "@/src/security/models/Permission";
import {
  Atom,
  Building,
  Building2,
  FileText,
  LockKeyhole,
  MessageSquare,
  Plus,
  Settings,
  Store,
  UserPlus,
} from "lucide-react";
import Image from "next/image";
import Link from "next/link";

interface Props {
  className?: string;
  pathname?: string;
}

interface Route {
  icon: any;
  href: string;
  pathname?: string;
  searchparams?: Record<string, string>;
  searchparamsregex?: Record<string, RegExp>;
  label: string;
  pro: boolean;
  regex: RegExp;
  requiredPermission?: Permission;
  children?: Route[];
}

export const PublicSidebar = ({ className, pathname = "/" }: Props) => {
  const shouldHideRoute = (route: Route) => {
    return false;
  };

  const onNavigate = (href: string, pro: boolean) => {};

  const routes = [
    {
      icon: Store,
      label: "Browse",
    },
    {
      icon: Plus,
      label: "Create",
    },
    {
      icon: Atom,
      label: "Your AIs",
    },
    {
      icon: UserPlus,
      label: "Shared",
    },
    {
      icon: FileText,
      label: "Data",
    },
    {
      icon: Building2,
      label: "Admin",
    },
    {
      icon: Settings,
      label: "Settings",
      children: [
        {
          icon: Building,
          label: "Company Settings",
        },
        {
          icon: LockKeyhole,
          label: "API Keys",
        },
      ],
    },
  ] as Route[];
  const itemClass =
    "text-muted-foreground text-xs group py-3 px-8 flex w-full justify-center font-medium cursor-pointer hover:text-primary hover:bg-primary/10 rounded-lg transition";
  return (
    <div
      className={cn(
        "p-3 flex-1 flex justify-between flex-col h-full",
        className
      )}
    >
      <div className="space-y-2 flex flex-col items-center">
        <div className="w-16">
          <Link href="/sign-up">
            <Image
              src="/appdirect-blue-gradient.jpeg"
              alt="AppDirect Logo"
              width="64"
              height="64"
              className="rounded-lg"
            />
          </Link>
        </div>
        <div
          className={cn(
            "text-muted-foreground text-xs group py-3 px-8 flex w-full justify-center font-medium rounded-lg transition",
            pathname.startsWith("/chat/")
              ? "bg-accent text-primary cursor-pointer hover:text-primary hover:bg-primary/10"
              : "cursor-pointer hover:text-primary hover:bg-primary/10"
          )}
        >
          <div className="flex flex-col items-center flex-1">
            <MessageSquare className="h-5 w-5 mb-1" />
            Chat
          </div>
        </div>
        {routes.map((route) => (
          <div key={route.href} className={cn(itemClass)}>
            <Link href="/sign-up">
              <div className="flex flex-col items-center flex-1">
                <route.icon className="h-5 w-5 mb-1" />
                <span className="w-12 text-center">{route.label}</span>
              </div>
            </Link>
          </div>
        ))}
      </div>
      <div className="space-y-2 flex flex-col items-center py-3 px-8">
        <ModeToggle />
      </div>
    </div>
  );
};
