import { Navbar } from "@/components/navbar";
import { Sidebar } from "@/components/sidebar";
import { checkSubscription } from "@/lib/subscription";
import { auth } from "@clerk/nextjs";
import prismadb from "@/lib/prismadb";

const RootLayout = async ({ children }: { children: React.ReactNode }) => {
  const isPro = await checkSubscription();
  const { userId } = auth();

  if (!userId) {
    return;
  }

  const conversations = await prismadb.companion.groupBy({
    by: ["id"],
    where: {
      messages: {
        some: {
          userId: userId,
        },
      },
    },
  });
  const lastChat = conversations.length > 0 ? conversations[0].id : undefined;

  return (
    <div className="h-full">
      <Navbar isPro={isPro} />
      <div className="hidden md:flex h-full w-20 flex-col fixed inset-y-0 z-40">
        <Sidebar isPro={isPro} lastChat={lastChat} />
      </div>
      <main className="md:pl-20 pt-20 md:pt-0 h-full">{children}</main>
    </div>
  );
};

export default RootLayout;
