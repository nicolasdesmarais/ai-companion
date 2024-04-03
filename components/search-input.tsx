"use client";

import { Input } from "@/components/ui/input";
import {
  Select,
  SelectContent,
  SelectItem,
  SelectTrigger,
  SelectValue,
} from "@/components/ui/select";
import { useDebounce } from "@/hooks/use-debounce";
import { Search } from "lucide-react";
import { useRouter, useSearchParams } from "next/navigation";
import qs from "query-string";
import { ChangeEventHandler, useEffect, useState } from "react";
import {useClerk} from "@clerk/nextjs";
import {routesHref} from "@/components/sidebar";
import axios from "axios";

const filterOptions = [
  { id: "popularity", name: "Popularity" },
  { id: "newest", name: "Newest" },
  { id: "rating", name: "Rating" },
];

const defaultSortValueforPath = {
  [routesHref.sharedHref] : "newest",
  [routesHref.yourAIHref] : "newest",
  [routesHref.browseHref] : "popularity"
}

export interface UserMetaDataInterface {
  userId: string;
  sortValue: string;
}

export interface ClerkUserIdInterface {
  clerkUserId: string
}

export const SearchInput = () => {
  const router = useRouter();
  const searchParams = useSearchParams();

  const categoryId = searchParams.get("categoryId");
  const search = searchParams.get("search");
  const sortParam = searchParams.get("sort");

  const [value, setValue] = useState(search || "");
  const debouncedValue = useDebounce<string>(value, 500);
  const [sort, setSort] = useState<string | undefined>(sortParam || "");
  const clerk = useClerk();
  const [defaultSortValue, setDefaultSortValue] = useState<string | undefined>(defaultSortValueforPath[window.location.pathname]);

  async function getSortValuefromPublicMetaData() {
    if (!clerk.user?.id) { return; }
    const request : ClerkUserIdInterface = {
      clerkUserId: clerk.user?.id
    }
    const response = await axios.post("/api/v1/clerk/user", request);
    console.log("Initial Sort Value from Public MetaData: ", response.data.user.publicMetadata.sort, " Value of Sort ", sort)
    setSort(response.data.user.publicMetadata.sort)
  }

  async function setSortInPublicMetaData(val : string) {
    setSort(val)
    if (!clerk.user?.id || !sort) { return;}
    const request : UserMetaDataInterface = {
      sortValue: sort,
      userId: clerk.user?.id
    }
    await axios.post("/api/v1/clerk", request);
    console.log("Sort Value Updated in Public MetaData: ", sort)
  }

  useEffect(() => {
    try {
      getSortValuefromPublicMetaData();
    } catch (e) {
      setDefaultSortValue(defaultSortValueforPath[window.location.pathname])
    }
  }, [sort, defaultSortValue, getSortValuefromPublicMetaData]);

  const onChange: ChangeEventHandler<HTMLInputElement> = (e) => {
    setValue(e.target.value);
  };

  useEffect(() => {
    const query = {
      search: debouncedValue,
      categoryId: categoryId,
      sort,
    };

    const url = qs.stringifyUrl(
      {
        url: window.location.href,
        query,
      },
      { skipNull: true, skipEmptyString: true }
    );

    router.push(url);
  }, [debouncedValue, router, categoryId, sort]);

  return (
    <div className="flex">
      <div className="relative flex-auto">
        <Search className="absolute h-4 w-4 top-3 left-4 text-muted-foreground" />
        <Input
          onChange={onChange}
          value={value}
          placeholder="Search..."
          className="pl-10 bg-accent"
        />
      </div>
      <Select
        onValueChange={(val) => setSortInPublicMetaData(val)}
        value={sort || defaultSortValue}
      >
        <SelectTrigger className="bg-accent w-32 md:w-44 ml-4 flex-none">
          <span className="hidden md:inline">Sort By:</span>
          <SelectValue />
        </SelectTrigger>
        <SelectContent>
          {filterOptions.map((f) => (
            <SelectItem key={f.id} value={f.id}>
              {f.name}
            </SelectItem>
          ))}
        </SelectContent>
      </Select>
    </div>
  );
};
