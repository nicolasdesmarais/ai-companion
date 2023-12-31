import { Navbar } from "@/components/navbar";
import { Sidebar } from "@/components/sidebar";
import prismadb from "@/src/lib/prismadb";
import { checkSubscription } from "@/src/lib/subscription";
import { getUserAuthorizationContext } from "@/src/security/utils/securityUtils";

const RootLayout = async ({ children }: { children: React.ReactNode }) => {
  const isPro = await checkSubscription();
  const authorizationContext = getUserAuthorizationContext();
  if (!authorizationContext) {
    return;
  }

  const { userId, permissions } = authorizationContext;
  if (!userId) {
    return;
  }

  const chats = await prismadb.chat.findMany({
    where: {
      userId: userId,
      isDeleted: false,
    },
  });

  return (
    <div className="h-full">
      <Navbar
        isPro={isPro}
        hasChat={chats.length > 0}
        userPermissions={permissions}
      />
      <div className="hidden md:flex h-full w-20 flex-col fixed inset-y-0 z-40">
        <Sidebar
          isPro={isPro}
          hasChat={chats.length > 0}
          userPermissions={permissions}
        />
      </div>
      <main className="md:pl-20 pt-20 md:pt-0 h-full">{children}</main>
    </div>
  );
};

export default RootLayout;
