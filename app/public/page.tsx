import { PublicAiListing } from "@/components/public-ai-listing";
import { serializeQueryParams } from "@/src/lib/utils";
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
    const query = serializeQueryParams(searchParams);
    return redirect(`/index/public?${query}`);
  }

  return <PublicAiListing searchParams={searchParams} />;
};

export default PublicIndex;
