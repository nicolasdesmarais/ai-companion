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

export default async function RootPage({ searchParams }: Props) {
  return <AiListing searchParams={searchParams} />;
}
