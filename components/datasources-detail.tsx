"use client";
import { ConfirmModal } from "@/components/confirm-modal";
import { DataRefreshPeriod } from "@/components/data-refresh-period";
import { DataSourceTypes } from "@/components/datasource-types";
import { Drawer } from "@/components/drawer";
import { TestChat } from "@/components/test-chat";
import { Avatar, AvatarImage } from "@/components/ui/avatar";
import { Badge } from "@/components/ui/badge";
import { Button } from "@/components/ui/button";
import {
  Form,
  FormDescription,
  FormItem,
  FormLabel,
} from "@/components/ui/form";
import { MultiSelect } from "@/components/ui/multi-select";
import { useToast } from "@/components/ui/use-toast";
import { useConfirmModal } from "@/hooks/use-confirm-modal";
import { zodResolver } from "@hookform/resolvers/zod";
import { DataSourceRefreshPeriod } from "@prisma/client";
import axios, { AxiosError } from "axios";
import { format } from "date-fns";
import { ArrowUpRightSquare, X } from "lucide-react";
import Link from "next/link";
import { useRouter, useSearchParams } from "next/navigation";
import qs from "query-string";
import { useEffect, useState } from "react";
import { useForm } from "react-hook-form";
import * as z from "zod";

interface Props {
  dataSources: any[];
  onChange: () => void;
}

export const DataSourcesDetails = ({ dataSources, onChange }: Props) => {
  const [ais, setAis] = useState<any[]>([]);
  const [loading, setLoading] = useState(false);
  const [selectedValues, setSelectedValues] = useState<any[]>([]);
  const [dataRefreshPeriod, setDataRefreshPeriod] =
    useState<DataSourceRefreshPeriod | null>(DataSourceRefreshPeriod.NEVER);
  const [chatOpen, setChatOpen] = useState(false);
  const [messages, setMessages] = useState<any[]>([]);
  const [chatAi, setChatAi] = useState<any>();
  const { toast } = useToast();
  const router = useRouter();
  const searchParams = useSearchParams();
  const focus = searchParams.get("focus");
  const dataSource = dataSources.find((ds) => ds.id === focus);
  const confirmModal = useConfirmModal();

  useEffect(() => {
    const fetchAis = async () => {
      try {
        const response = await axios.get(`/api/v1/me/ai?scope=OWNED`);
        setAis(response.data);
      } catch (error: any) {
        toast({
          variant: "destructive",
          description:
            String((error as AxiosError).response?.data) ||
            "Something went wrong.",
          duration: 6000,
        });
      }
    };
    fetchAis();
  }, [toast]);

  useEffect(() => {
    if (dataSource?.ais) {
      setSelectedValues(dataSource.ais.map((ai: any) => ai.ai));
      setDataRefreshPeriod(
        dataSource.refreshPeriod || DataSourceRefreshPeriod.NEVER
      );
    }
  }, [dataSource]);

  const form = useForm<any>({
    resolver: zodResolver(z.object({})),
    defaultValues: {},
  });

  const onSubmit = async (values: any) => {
    try {
      setLoading(true);
      await axios.patch(`/api/v1/data-sources/${dataSource.id}`, values);
      toast({
        description: "Data source updated",
      });
      onChange();
    } catch (error) {
      console.error(error);
      toast({
        description: "Something went wrong",
        variant: "destructive",
      });
    } finally {
      setLoading(false);
    }
  };

  const handleDelete = async () => {
    try {
      const response = await axios.delete(
        `/api/v1/data-sources/${dataSource.id}`
      );
      if (response.status === 204) {
        toast({
          description: "Data Source deleted successfully",
        });
      } else {
        throw new Error(response.data.message);
      }
      const url = qs.stringifyUrl(
        {
          url: window.location.href,
          query: {
            focus: null,
          },
        },
        { skipNull: true, skipEmptyString: true }
      );
      router.push(url);
      onChange();
    } catch (error) {
      toast({
        description: "Something went wrong",
        variant: "destructive",
      });
    }
  };

  if (!focus || !dataSource || !dataSources || dataSources.length === 0)
    return null;

  return (
    <div className="bg-profile md:ml-2 md:mt-2 p-4 md:min-w-[500px] w-full md:w-1/3 absolute md:sticky top-[64px] bottom-0">
      <div className="absolute top-2 right-4">
        <Button
          onClick={() => {
            const url = qs.stringifyUrl(
              {
                url: window.location.href,
                query: {
                  focus: null,
                },
              },
              { skipNull: true, skipEmptyString: true }
            );
            router.push(url);
          }}
          variant="ghost"
          size="icon"
          type="button"
        >
          <X className="h-6 w-6" />
        </Button>
      </div>
      <div className="text-xl font-bold pr-8 truncate">{dataSource.name}</div>

      <Form {...form}>
        <form className="space-y-4 mt-4">
          <FormItem>
            <FormLabel className="uppercase">Selected AIs</FormLabel>
            {ais && (
              <>
                <div className="relative mt-3 overflow-y-auto">
                  {selectedValues.map((ai, index) => (
                    <Badge
                      key={`badge-${dataSource.id}-${index}`}
                      variant="outline"
                      className="mr-2 mb-2 bg-ring cursor-pointer hover:bg-ring/80"
                      onClick={() => {
                        setMessages([]);
                        setChatAi(ai);
                        setChatOpen(true);
                      }}
                    >
                      <Avatar className="h-6 w-6 mr-2">
                        <AvatarImage src={ai.src} crop="w_48,h_48" />
                      </Avatar>
                      {ai.name}
                    </Badge>
                  ))}
                </div>
                <MultiSelect
                  itemLabel="AI"
                  items={ais}
                  values={selectedValues}
                  onChange={(values) => {
                    setSelectedValues(values);
                    onSubmit({ ais: values.map((ai) => ai.id) });
                  }}
                />
                <FormDescription>
                  Select the AIs you would like to have access to this data.
                </FormDescription>
              </>
            )}
          </FormItem>
          <DataRefreshPeriod
            selectClassName="max-w-[200px]"
            labelClassName="uppercase"
            setDataRefreshPeriod={(value) => {
              if (value) {
                setDataRefreshPeriod(value);
                onSubmit({ refreshPeriod: value });
              }
            }}
            dataRefreshPeriod={dataRefreshPeriod}
          />
          <FormItem>
            <FormLabel className="uppercase">Type</FormLabel>
            <FormDescription>
              {
                DataSourceTypes.find(
                  (format) => format.type === dataSource.type
                )?.name
              }
            </FormDescription>
          </FormItem>
          <FormItem>
            <FormLabel className="uppercase">Original Upload</FormLabel>
            <FormDescription>
              {format(new Date(dataSource.createdAt), "h:mma M/d/yyyy ")}
            </FormDescription>
          </FormItem>
          <FormItem>
            <FormLabel className="uppercase">Last Indexed</FormLabel>
            <FormDescription>
              {format(new Date(dataSource.lastIndexedAt), "h:mma M/d/yyyy ")}
            </FormDescription>
          </FormItem>
          {dataSource.knowledges.length > 0 && (
            <FormItem>
              <FormLabel className="uppercase">Content</FormLabel>
              {dataSource.knowledges.map(({ knowledge }: any) => (
                <div className="flex justify-between" key={knowledge.id}>
                  {knowledge.blobUrl ? (
                    <div>
                      <a
                        href={knowledge.blobUrl}
                        target="_blank"
                        className="text-ring"
                      >
                        {knowledge.name}
                      </a>
                    </div>
                  ) : (
                    <FormDescription>{knowledge.name}</FormDescription>
                  )}
                  <div>
                    {knowledge.indexPercentage
                      ? `(${knowledge.indexPercentage.toFixed(1)}% indexed)`
                      : ""}
                  </div>
                </div>
              ))}
            </FormItem>
          )}
        </form>
      </Form>
      <Button
        onClick={() =>
          confirmModal.onOpen(
            "Delete Data Source?",
            <div>
              <div>Are you sure you want to delete {dataSource.name}?</div>
              <div>This action cannot be undone.</div>
            </div>,
            handleDelete
          )
        }
        type="button"
        variant="destructive"
        className="mt-8"
      >
        Delete Data
      </Button>
      <Drawer open={chatOpen} setOpen={setChatOpen}>
        <TestChat
          ai={chatAi}
          messages={messages}
          setMessages={setMessages}
          actions={
            <div className="flex justify-between">
              <Button
                onClick={() => {
                  setMessages([]);
                }}
                type="button"
              >
                Restart Chat
              </Button>
              <Link
                href={`/ai/${chatAi?.id}/edit`}
                rel="noopener noreferrer"
                target="_blank"
              >
                <Button variant="ring" type="button">
                  Open AI Editor
                  <ArrowUpRightSquare className="h-4 w-4 ml-2" />
                </Button>
              </Link>
            </div>
          }
        />
      </Drawer>
      <ConfirmModal />
    </div>
  );
};
