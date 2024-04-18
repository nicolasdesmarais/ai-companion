"use client";
import { Button } from "@/components/ui/button";
import { useToast } from "@/components/ui/use-toast";
import axios, { AxiosError } from "axios";
import { useEffect, useState } from "react";
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
          <div key={indexDescription.name} className="pt-4">
            <h3 className="text-lg font-medium">{indexDescription.name}</h3>
            {isCurrent ? (
              <Banner className="w-80 my-2">
                This environment is using this index.
              </Banner>
            ) : null}
            <div>Pods: {indexDescription.spec.pod.pods}</div>
            <div>Replicas: {indexDescription.spec.pod.replicas}</div>
            <div>Shards: {indexDescription.spec.pod.shards}</div>
            <div>Pod Type: {indexDescription.spec.pod.podType}</div>
            <div>Fullness: {indexStats.indexFullness}</div>
            <div>Total Record Count: {indexStats.totalRecordCount}</div>
            <Button className="mt-4" onClick={backup(indexDescription.name)}>
              Backup
            </Button>
          </div>
        ))}
      </div>

      <h3 className="text-lg font-medium">Pinecone Backups</h3>
      <div className="text-muted-foreground text-sm">
        {collections.map(({ name, size, status, vectorCount }: any) => (
          <div key={name} className="pt-4">
            <h3 className="text-lg font-medium">{name}</h3>
            <div>Size: {Math.round(size / 1000000)} Mb</div>
            <div>Status: {status}</div>
            <div>Vector Count: {vectorCount}</div>
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
