"use client";
import {
  FormControl,
  FormDescription,
  FormItem,
  FormLabel,
} from "@/components/ui/form";
import {
  Select,
  SelectContent,
  SelectItem,
  SelectTrigger,
  SelectValue,
} from "@/components/ui/select";
import { cn } from "@/src/lib/utils";
import { DataSourceRefreshPeriod } from "@prisma/client";
import { getDataSourceRefreshPeriodLabel } from "./datasource-refresh-periods";

interface Props {
  dataRefreshPeriod: DataSourceRefreshPeriod | null;
  setDataRefreshPeriod: (period: DataSourceRefreshPeriod) => void;
  className?: string;
  selectClassName?: string;
  labelClassName?: string;
}

export const DataRefreshPeriod = ({
  dataRefreshPeriod,
  setDataRefreshPeriod,
  className,
  selectClassName,
  labelClassName,
}: Props) => {
  return (
    <div className={cn("my-4", className)}>
      <FormItem>
        <FormLabel className={labelClassName}>Data Refresh Interval</FormLabel>
        <Select
          onValueChange={(value: DataSourceRefreshPeriod) =>
            setDataRefreshPeriod(value)
          }
          value={dataRefreshPeriod ?? ""}
        >
          <FormControl>
            <SelectTrigger className={cn("my-4", selectClassName)}>
              <SelectValue>
                {getDataSourceRefreshPeriodLabel(dataRefreshPeriod)}
              </SelectValue>
            </SelectTrigger>
          </FormControl>
          <SelectContent>
            {Object.values(DataSourceRefreshPeriod).map((period) => (
              <SelectItem key={period} value={period}>
                {getDataSourceRefreshPeriodLabel(period)}
              </SelectItem>
            ))}
          </SelectContent>
        </Select>
        <FormDescription>
          Determine how often your data source will be reindexed. Please be
          aware that this may increase costs.
        </FormDescription>
      </FormItem>
    </div>
  );
};
