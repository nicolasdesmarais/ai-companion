"use client";
import { Button } from "@/components/ui/button";
import { useToast } from "@/components/ui/use-toast";
import axios from "axios";
import { useState } from "react";
import { useRouter } from "next/navigation";
import { Globe, Loader } from "lucide-react";

interface WebUrlsProps {
  aiId: string;
}

export const WebUrlsForm = ({ aiId }: WebUrlsProps) => {
  const [loading, setLoading] = useState(false);
  const { toast } = useToast();
  const [urls, setUrls] = useState([""]);
  const router = useRouter();

  const handleContinue = async () => {
    setLoading(true);
    try {
      await axios.post(`/api/v1/ai/${aiId}/knowledge/web-urls`, { urls });
      setLoading(false);
      toast({
        variant: "default",
        description: "Web URLs added",
      });
      router.push(`/ai/${aiId}/edit/knowledge`);
    } catch (error) {
      console.log(error);
      toast({
        variant: "destructive",
        description: "Something went wrong",
      });
    }
  };

  const handleUrlChange = (index: number, value: string) => {
    const updatedUrls = [...urls];
    updatedUrls[index] = value;
    setUrls(updatedUrls);
  };

  return (
    <div className="w-full p-6 bg-gray-900 text-white">
      <div className="mb-4">
        <h2 className="text-xl font-bold">Website URLs</h2>
        <p className="text-gray-400">
          Choose URLs you want AppDirect AI to read to train this AIs knowledge
        </p>

        {urls.map((url, index) => (
          <input
            key={index}
            type="text"
            placeholder="https://google.com"
            className="mt-2 w-full p-2 bg-gray-800 border rounded border-gray-700"
            value={url}
            onChange={(e) => handleUrlChange(index, e.target.value)}
          />
        ))}

        <div className="mt-2">
          <a
            className="text-blue-500 cursor-pointer"
            onClick={() => setUrls([...urls, ""])}
          >
            + Add More
          </a>
        </div>
      </div>

      <div className="flex justify-between w-full">
        <Button
          onClick={handleContinue}
          disabled={!urls.some((url) => url)}
          variant="ring"
        >
          Load
          {loading ? (
            <Loader className="w-4 h-4 ml-2 spinner" />
          ) : (
            <Globe className="w-4 h-4 ml-2" />
          )}
        </Button>
      </div>
    </div>
  );
};
