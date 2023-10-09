import { Categories } from "@/components/categories";
import { Companions } from "@/components/companions";
import { SearchInput } from "@/components/search-input";
import { AIService } from "@/domain/services/AIService";
import { ListAIsRequestParams, ListAIsRequestScope } from "@/domain/services/dtos/ListAIsRequestParams";
import prismadb from "@/lib/prismadb";
import { auth } from "@clerk/nextjs";

interface RootPageProps {
  searchParams: {
    scope: string;
    groupId: string;
    categoryId: string;
    name: string;
  };
};

const RootPage = async ({
  searchParams
}: RootPageProps) => {
  const aiService = new AIService();
  const authorization = await auth();

  const scopeParam = searchParams.scope;
  let scope: ListAIsRequestScope | undefined;
  if (!scopeParam || !Object.values(ListAIsRequestScope).includes(scopeParam as ListAIsRequestScope)) {
    scope = undefined;
  } else {
    scope = ListAIsRequestScope[scopeParam as keyof typeof ListAIsRequestScope];
  }

  const requestParams: ListAIsRequestParams = {
    scope: scope,
    groupId: searchParams.groupId,
    categoryId: searchParams.categoryId,
    search: searchParams.name
  }

  const data = await aiService.findAIsForUser(authorization, requestParams);
  const categories = await prismadb.category.findMany();

  return (
    <div className="h-full px-4 space-y-2">
      <SearchInput />
      <Categories data={categories} />
      <Companions data={data} />
    </div>
  )
}

export default RootPage
