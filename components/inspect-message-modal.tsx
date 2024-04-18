"use client";

import { useEffect, useState } from "react";
import { Dialog, DialogContent } from "@/components/ui/dialog";
import { useInspectMessage } from "@/hooks/use-inspect-message";
import axios from "axios";
import { Loader } from "lucide-react";

interface Props {}

export const InspectMessageModal = ({}: Props) => {
  const [knowledge, setKnowledge] = useState<any>(null);
  const [sources, setSources] = useState<any[]>([]);
  const [isMounted, setIsMounted] = useState(false);
  const [isLoading, setIsLoading] = useState(false);
  const { isOpen, onClose, query, message, ai, messages } = useInspectMessage();

  useEffect(() => {
    setIsMounted(true);
  }, []);

  useEffect(() => {
    const fetchKnowledge = async () => {
      setIsLoading(true);
      const response = await axios.post(`/api/v1/ai/${ai.id}/knowledge`, {
        prompt: query,
        tokensUsed: message.metadata.tokensUsed,
        messages,
      });
      setKnowledge(response.data.knowledge);
      setSources(response.data.docMeta);
      setIsLoading(false);
    };
    if (query) {
      fetchKnowledge();
    }
  }, [query, message, ai, messages]);

  if (!isMounted) {
    return null;
  }

  return (
    <Dialog
      open={isOpen}
      onOpenChange={() => {
        setKnowledge(null);
        setSources([]);
        setIsLoading(true);
        onClose();
      }}
    >
      <DialogContent className="max-w-screen-2xl overflow-auto">
        <div className="text-xs">
          <div className="mt-8">Query: &quot;{query}&quot;</div>
          {message ? (
            <div className="mt-4">
              <div>Tokens Used: {message.metadata.tokensUsed || "unknown"}</div>
              <div>Total AI Time: {message.metadata.totalTime / 1000} sec</div>
              <div>AI Setup Time: {message.metadata.setupTime / 1000} sec</div>
              <div>
                Knowledge Retrieval Time:{" "}
                {message.metadata.knowledgeTime / 1000} sec
              </div>
              <div>LLM Time: {message.metadata.llmTime / 1000} sec</div>
            </div>
          ) : null}
          {knowledge ? (
            <div className="mt-4">
              <div>Knowledge used to answer query:</div>
              <div className="text-xxs w-full overflow-auto whitespace-pre">
                {knowledge}
              </div>
            </div>
          ) : null}
          {sources.length ? (
            <div className="mt-4">
              <div>Data Sources Used:</div>
              <div className="mt-4">
                {sources.map((source, index) => (
                  <div className="mt-2 flex gap-8" key={`source-${index}`}>
                    <div>{source.source}</div>
                    <div>{source.blobType}</div>
                    <div>{source.tokenCount} tokens</div>
                  </div>
                ))}
              </div>
            </div>
          ) : null}
          {isLoading ? <Loader className="mt-4 spinner" /> : null}
        </div>
      </DialogContent>
    </Dialog>
  );
};
