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
import { useClerk } from "@clerk/nextjs";
import axios from "axios";
import { Search } from "lucide-react";
import { useRouter, useSearchParams } from "next/navigation";
import qs from "query-string";
import { ChangeEventHandler, useEffect, useState } from "react";

const filterOptions = [
  { id: "popularity", name: "Popularity" },
  { id: "newest", name: "Newest" },
  { id: "rating", name: "Rating" },
];

export interface Props {
  scopeParam?: string;
}

export const SearchInput = ({ scopeParam }: Props) => {
  const clerk = useClerk();
  const router = useRouter();
  const searchParams = useSearchParams();

  const categoryId = searchParams.get("categoryId");
  const search = searchParams.get("search");
  const sortParam = searchParams.get("sort");

  const [value, setValue] = useState(search || "");
  const debouncedValue = useDebounce<string>(value, 500);
  const [sort, setSort] = useState<string | undefined>(sortParam || "");

  let defaultSort = "popularity";
  if (!scopeParam || scopeParam === "public") {
    defaultSort = "rating";
  } else if (scopeParam === "shared" || scopeParam === "owned") {
    defaultSort = "newest";
  }

  async function onSortChange(value: string) {
    setSort(value);
    await axios.post("/api/v1/clerk", {
      key: `sort${scopeParam ? "-" + scopeParam : ""}`,
      value,
      userId: clerk.user?.id,
    });
  }

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
        onValueChange={(val) => onSortChange(val)}
        value={sort || defaultSort}
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
