"use client";

import * as React from "react";
import { Check, ChevronsUpDown, Edit2 } from "lucide-react";

import { cn } from "@/src/lib/utils";
import { Button } from "@/components/ui/button";
import {
  Command,
  CommandGroup,
  CommandInput,
  CommandItem,
} from "@/components/ui/command";
import {
  Popover,
  PopoverContent,
  PopoverTrigger,
} from "@/components/ui/popover";
import { Avatar, AvatarImage } from "@/components/ui/avatar";

type Props = {
  itemLabel: string;
  items: any[];
  values: any[];
  setValues: React.Dispatch<React.SetStateAction<any[]>>;
};

export function MultiSelect({ itemLabel, items, values, setValues }: Props) {
  const inputRef = React.useRef<HTMLInputElement>(null);
  const [openCombobox, setOpenCombobox] = React.useState(false);
  const [inputValue, setInputValue] = React.useState<string>("");
  const toggleItem = (item: any) => {
    setValues((currentItems) =>
      !values.find((l) => l.id === item.id)
        ? [...currentItems, item]
        : currentItems.filter((l) => l.id !== item.id)
    );
    inputRef?.current?.focus();
  };

  const onComboboxOpenChange = (value: boolean) => {
    inputRef.current?.blur(); // HACK: otherwise, would scroll automatically to the bottom of page
    setOpenCombobox(value);
  };

  return (
    <div className="max-w-[200px]">
      <Popover open={openCombobox} onOpenChange={onComboboxOpenChange}>
        <PopoverTrigger asChild>
          <Button
            variant="outline"
            role="combobox"
            aria-expanded={openCombobox}
            className="w-[200px] justify-between text-foreground"
          >
            <span className="truncate">
              {values.length === 0 && `Select ${itemLabel}`}
              {values.length === 1 && values[0].name}
              {values.length === 2 && values.map(({ name }) => name).join(", ")}
              {values.length > 2 && `${values.length} ${itemLabel}s selected`}
            </span>
            <ChevronsUpDown className="ml-2 h-4 w-4 shrink-0 opacity-50" />
          </Button>
        </PopoverTrigger>
        <PopoverContent className="w-[200px] p-0">
          <Command loop>
            <CommandInput
              ref={inputRef}
              placeholder={`Search ${itemLabel}s...`}
              value={inputValue}
              onValueChange={setInputValue}
            />
            <CommandGroup className="max-h-[145px] overflow-auto">
              {items.map((item) => {
                const isActive = values.find((l) => l.id === item.id);
                return (
                  <CommandItem
                    key={item.id}
                    value={item.id}
                    onSelect={() => toggleItem(item)}
                  >
                    <Check
                      className={cn(
                        "mr-2 h-4 w-4",
                        isActive ? "opacity-100" : "opacity-0"
                      )}
                    />
                    <div className="flex-1">{item.name}</div>
                    {item.src && (
                      <Avatar className="h-4 w-4">
                        <AvatarImage src={item.src} crop="w_48,h_48" />
                      </Avatar>
                    )}
                  </CommandItem>
                );
              })}
            </CommandGroup>
          </Command>
        </PopoverContent>
      </Popover>
    </div>
  );
}
