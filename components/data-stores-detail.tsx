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
import { Button } from "./ui/button";
import { X } from "lucide-react";

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
  const dataSource = dataSources.find((ds) => ds.id === focus);

  if (!focus || !dataSource || !dataSources || dataSources.length === 0)
    return null;

  return (
    <div className="bg-profile ml-2 mt-2 mw-1/3 p-4">
      <div className="absolute top-[110px] right-4">
        <Button onClick={() => {}} variant="ghost" size="icon" type="button">
          <X className="h-6 w-6" />
        </Button>
      </div>
      <div className="text-xl font-bold">{dataSource.name}</div>

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
            <FormLabel>Type</FormLabel>
            <div>{dataSource.type}</div>
          </FormItem>
          <FormItem>
            <FormLabel>Original Upload</FormLabel>
            <div>{dataSource.createdAt}</div>
          </FormItem>
          <FormItem>
            <FormLabel>Last Modified</FormLabel>
            <div>{dataSource.updatedAt}</div>
          </FormItem>
          <FormItem>
            <FormLabel>Content</FormLabel>
            {dataSource.knowledges.map(({ knowledge }: any) =>
              knowledge.blobUrl ? (
                <div key={knowledge.id}>
                  <a
                    href={knowledge.blobUrl}
                    target="_blank"
                    className="text-ring"
                  >
                    {knowledge.name}
                  </a>
                </div>
              ) : (
                <div key={knowledge.id}>{knowledge.name}</div>
              )
            )}
          </FormItem>
        </form>
      </Form>
    </div>
  );
};
