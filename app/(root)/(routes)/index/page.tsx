import { AiListing } from "@/components/ai-listing";

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

const IndexPage = async ({ searchParams }: Props) => {
  return <AiListing searchParams={searchParams} />;
};

export default IndexPage;
