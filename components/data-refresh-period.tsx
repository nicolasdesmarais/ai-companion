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
import { DataSourceRefreshPeriod } from "@prisma/client";
import { getDataSourceRefreshPeriodLabel } from "./datasource-refresh-periods";

interface Props {
  dataRefreshPeriod: DataSourceRefreshPeriod | null;
  setDataRefreshPeriod: (period: DataSourceRefreshPeriod) => void;
}

export const DataRefreshPeriod = ({
  dataRefreshPeriod,
  setDataRefreshPeriod,
}: Props) => {
  return (
    <div className="my-4">
      <FormItem>
        <FormLabel>Data Refresh Interval</FormLabel>
        <Select
          onValueChange={(value: DataSourceRefreshPeriod) =>
            setDataRefreshPeriod(value)
          }
          value={dataRefreshPeriod ?? ""}
        >
          <FormControl>
            <SelectTrigger className="bg-background">
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
