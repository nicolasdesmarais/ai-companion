import { PublicAiListing } from "@/components/public-ai-listing";

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
  return <PublicAiListing searchParams={searchParams} />;
};

export default PublicIndex;
