"use client";
import { Button } from "@/components/ui/button";
import axios from "axios";
import { redirect } from "next/navigation";
import { useState } from "react";

interface WebUrlsProps {
  aiId: string;
}

export const WebUrlsForm = ({ aiId }: WebUrlsProps) => {
  const [dataStoreName, setDataStoreName] = useState("");
  const [url, setUrl] = useState("");

  const handleContinue = async () => {
    axios.post(`/api/v1/ai/${aiId}/knowledge/web-urls`, { url });
  };

  return (
    <div className="w-full p-6 bg-gray-900 text-white">
      <div className="mb-4">
        <h2 className="text-xl font-bold">Data Store Name</h2>
        <p className="text-gray-400">
          Name this data set so you can use it later for other AIs. Choose
          something descriptive
        </p>
        <input
          type="text"
          placeholder="My business website index"
          className="mt-2 w-full p-2 bg-gray-800 border rounded border-gray-700"
          value={dataStoreName}
          onChange={(e) => setDataStoreName(e.target.value)}
        />
      </div>
      <div className="mb-4">
        <h2 className="text-xl font-bold">Website URLs</h2>
        <p className="text-gray-400">
          Choose URLs you want AppDirect AI to read to train this AIs knowledge
        </p>
        <input
          type="text"
          placeholder="https://google.com"
          className="mt-2 w-full p-2 bg-gray-800 border rounded border-gray-700"
          value={url}
          onChange={(e) => setUrl(e.target.value)}
        />
      </div>
      <div className="flex justify-between w-full">
        <Button onClick={handleContinue} disabled={!url} variant="ring">
          Continue
        </Button>
        <Button
          onClick={() => redirect(`/ai/${aiId}/knowledge`)}
          variant="link"
        >
          Back
        </Button>
      </div>
    </div>
  );
};
