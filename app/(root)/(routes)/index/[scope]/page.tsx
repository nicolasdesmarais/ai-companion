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
  params: {
    scope: string;
  };
}

const IndexPage = async ({ searchParams, params }: Props) => {
  return <AiListing searchParams={searchParams} scopeParam={params.scope} />;
};

export default IndexPage;
