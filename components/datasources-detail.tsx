"use client";
import { DataRefreshPeriod } from "@/components/data-refresh-period";
import { Drawer } from "@/components/drawer";
import { TestChat } from "@/components/test-chat";
import { Avatar, AvatarImage } from "@/components/ui/avatar";
import { Badge } from "@/components/ui/badge";
import { Button } from "@/components/ui/button";
import { Form, FormItem, FormLabel } from "@/components/ui/form";
import { MultiSelect } from "@/components/ui/multi-select";
import { useToast } from "@/components/ui/use-toast";
import { zodResolver } from "@hookform/resolvers/zod";
import { DataSourceRefreshPeriod } from "@prisma/client";
import axios, { AxiosError } from "axios";
import { ArrowUpRightSquare, X } from "lucide-react";
import Link from "next/link";
import { useSearchParams } from "next/navigation";
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

  const searchParams = useSearchParams();
  const focus = searchParams.get("focus");
  const dataSource = dataSources.find((ds) => ds.id === focus);

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

  if (!focus || !dataSource || !dataSources || dataSources.length === 0)
    return null;

  return (
    <div className="bg-profile ml-2 mt-2 w-1/3 p-4">
      <div className="absolute top-[110px] right-4">
        <Button onClick={() => {}} variant="ghost" size="icon" type="button">
          <X className="h-6 w-6" />
        </Button>
      </div>
      <div className="text-xl font-bold pr-8 truncate">{dataSource.name}</div>

      <Form {...form}>
        <form className="space-y-4 mt-4">
          <FormItem>
            <FormLabel>Selected AIs</FormLabel>
            {ais && (
              <>
                <div className="relative mt-3 overflow-y-auto">
                  {selectedValues.map((ai) => (
                    <Badge
                      key={`badge-${ai.id}`}
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
              </>
            )}
          </FormItem>
          <DataRefreshPeriod
            selectClassName="max-w-[200px]"
            setDataRefreshPeriod={(value) => {
              if (value) {
                setDataRefreshPeriod(value);
                onSubmit({ refreshPeriod: value });
              }
            }}
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
          {dataSource.knowledges.length > 0 && (
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
          )}
        </form>
      </Form>
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
                href={`/ai/${chatAi.id}/edit`}
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
    </div>
  );
};
