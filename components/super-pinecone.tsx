"use client";
import { useEffect } from "react";
import { Table } from "@/components/table";
import { Button } from "@/components/ui/button";
import { useToast } from "@/components/ui/use-toast";
import axios, { AxiosError } from "axios";
import { format } from "date-fns";
import { ChevronRight, ChevronDown, Loader, MinusCircle } from "lucide-react";
import { useState } from "react";
import { DataSourceTypes } from "./datasource-types";
import { KnowledgeIndexStatus } from "@prisma/client";
import Link from "next/link";
import { Banner } from "./ui/banner";

export const SuperPinecone = () => {
  const [indexes, setIndexes] = useState<any[]>([]);
  const [collections, setCollections] = useState<any[]>([]);
  const { toast } = useToast();

  const fetchPineconeState = async () => {
    try {
      const response = await axios.get(`/api/v1/super/pinecone`);
      setIndexes(response.data.indexes);
      setCollections(response.data.collections);
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

  const backup = (indexName: string) => async () => {
    try {
      const response = await axios.get(
        `/api/v1/super/pinecone/${indexName}/backup`
      );
      fetchPineconeState();
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

  const deleteCollection = (collectionName: string) => async () => {
    try {
      const response = await axios.delete(
        `/api/v1/super/pinecone/collection/${collectionName}`
      );
      fetchPineconeState();
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

  useEffect(() => {
    fetchPineconeState();
  }, []);

  return (
    <div className="h-full p-4 space-y-2">
      <h2 className="text-lg font-medium">Pinecone Indexes</h2>
      <div className="text-muted-foreground text-sm">
        {indexes.map(({ indexDescription, indexStats, isCurrent }: any) => (
          <div key={indexDescription.database.name} className="pt-4">
            <h3 className="text-lg font-medium">
              {indexDescription.database.name}
            </h3>
            {isCurrent ? (
              <Banner className="w-80 my-2">
                This environment is using this index.
              </Banner>
            ) : null}
            <div>Pods: {indexDescription.database.pods}</div>
            <div>Replicas: {indexDescription.database.replicas}</div>
            <div>Shards: {indexDescription.database.shards}</div>
            <div>Pod Type: {indexDescription.database.podType}</div>
            <div>Fullness: {indexStats.indexFullness}</div>
            <div>Total Record Count: {indexStats.totalRecordCount}</div>
            <Button
              className="mt-4"
              onClick={backup(indexDescription.database.name)}
            >
              Backup
            </Button>
          </div>
        ))}
      </div>

      <h3 className="text-lg font-medium">Pinecone Backups</h3>
      <div className="text-muted-foreground text-sm">
        {collections.map(({ name, size, status, recordCount }: any) => (
          <div key={name} className="pt-4">
            <h3 className="text-lg font-medium">{name}</h3>
            <div>Size: {Math.round(size / 1000000)} Mb</div>
            <div>Status: {status}</div>
            <div>Vector Count: {recordCount}</div>
            <Button
              className="mt-4"
              variant="destructive"
              onClick={deleteCollection(name)}
            >
              Delete Backup
            </Button>
          </div>
        ))}
      </div>
    </div>
  );
};
