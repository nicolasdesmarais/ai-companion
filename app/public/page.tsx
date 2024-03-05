import { PublicAiListing } from "@/components/public-ai-listing";
import { auth } from "@clerk/nextjs";
import { redirect } from "next/navigation";

interface Props {
  searchParams: {
    scope: string;
    groupId: string;
    categoryId: string;
    approvedByOrg: string;
    search: string;
    sort: string;
  };
}

const PublicIndex = ({ searchParams }: Props) => {
  const { userId } = auth();

  if (userId) {
    return redirect("/index/public");
  }

  return <PublicAiListing searchParams={searchParams} />;
};

export default PublicIndex;
