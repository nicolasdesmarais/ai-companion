import { PublicSidebar } from "./public-sidebar";

interface Props {
  aiId: string;
}

export const PublicAiChat = async ({}: Props) => {
  return (
    <div className="h-full">
      <div className="hidden md:flex h-full w-20 flex-col fixed inset-y-0 z-40">
        <PublicSidebar isChat={true} />
      </div>
      <main className="md:pl-20 pt-20 md:pt-0 h-full">haha</main>
    </div>
  );
};
