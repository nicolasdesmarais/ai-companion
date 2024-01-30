"use client";
import { useSearchParams } from "next/navigation";
import { DataRefreshPeriod } from "./data-refresh-period";
import { useState } from "react";
import { DataSourceRefreshPeriod } from "@prisma/client";
import { Controller, useForm } from "react-hook-form";
import * as z from "zod";
import { zodResolver } from "@hookform/resolvers/zod";
import { useToast } from "@/components/ui/use-toast";
import axios, { AxiosError } from "axios";
import {
  Form,
  FormControl,
  FormField,
  FormItem,
  FormLabel,
  FormMessage,
} from "@/components/ui/form";

interface Props {
  dataSources: any[];
}
interface DataSourceFormData {
  name: string;
  scopes: string[];
}

const dataSourceFormSchema = z.object({
  name: z.string().min(1, { message: "Name is required." }),
});

export const DataStoresDetails = ({ dataSources }: Props) => {
  const [loading, setLoading] = useState(false);
  const [dataRefreshPeriod, setDataRefreshPeriod] =
    useState<DataSourceRefreshPeriod | null>(DataSourceRefreshPeriod.NEVER);
  const { toast } = useToast();

  const form = useForm<DataSourceFormData>({
    resolver: zodResolver(dataSourceFormSchema),
    defaultValues: {
      name: "",
      scopes: [],
    },
  });
  const searchParams = useSearchParams();
  const focus = searchParams.get("focus");

  const onSubmit = async (values: DataSourceFormData) => {
    try {
      setLoading(true);
      await axios.post("/api/v1/data-store", {});
    } catch (error) {
      toast({
        description: "Something went wrong",
        variant: "destructive",
      });
    } finally {
      setLoading(false);
    }
  };

  if (!focus || !dataSources || dataSources.length === 0) return null;
  const dataSource = dataSources.find((ds) => ds.id === focus);
  return (
    <div className="mt-2 w-1/3 p-4">
      <div>{dataSource.name}</div>

      <Form {...form}>
        <form onSubmit={form.handleSubmit(onSubmit)} className="space-y-4">
          <FormItem>
            <FormLabel>Selected AIs</FormLabel>
          </FormItem>
          <DataRefreshPeriod
            setDataRefreshPeriod={setDataRefreshPeriod}
            dataRefreshPeriod={dataRefreshPeriod}
          />
          <FormItem>
            <FormLabel>Original Name</FormLabel>
          </FormItem>
          <FormItem>
            <FormLabel>Type</FormLabel>
          </FormItem>
          <FormItem>
            <FormLabel>Original Upload</FormLabel>
          </FormItem>
          <FormItem>
            <FormLabel>Last Modified</FormLabel>
          </FormItem>
        </form>
      </Form>
    </div>
  );
};
