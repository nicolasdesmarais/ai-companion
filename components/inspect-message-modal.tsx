"use client";

import { useEffect, useState } from "react";
import { Dialog, DialogContent } from "@/components/ui/dialog";
import { useInspectMessage } from "@/hooks/use-inspect-message";
import axios from "axios";

interface Props {}

export const InspectMessageModal = ({}: Props) => {
  const [knowledge, setKnowledge] = useState<any>(null);
  const [sources, setSources] = useState<any[]>([]);
  const [isMounted, setIsMounted] = useState(false);
  const { isOpen, onClose, query, message, ai, messages } = useInspectMessage();

  useEffect(() => {
    setIsMounted(true);
  }, []);

  useEffect(() => {
    const fetchKnowledge = async () => {
      const response = await axios.post(`/api/v1/ai/${ai.id}/knowledge`, {
        prompt: query,
        tokensUsed: message.metadata.tokensUsed,
        messages,
      });
      setKnowledge(response.data.knowledge);
      setSources(response.data.docMeta);
    };
    if (query) {
      fetchKnowledge();
    }
  }, [query]);

  if (!isMounted) {
    return null;
  }

  return (
    <Dialog open={isOpen} onOpenChange={() => onClose()}>
      <DialogContent className="max-w-screen-2xl">
        <div className="text-xs">
          <div className="mt-8">Query: "{query}"</div>
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
              <div>Knowledge available to answer query:</div>
              <pre className="text-xxs w-full">{knowledge}</pre>
            </div>
          ) : null}
          {sources.length ? (
            <div className="mt-4">
              <div>Data Sources Used:</div>
              <div className="mt-4">
                {sources.map((source) => (
                  <div className="mt-2 flex gap-8">
                    <div>{source.source}</div>
                    <div>{source.blobType}</div>
                    <div>{source.tokenCount} tokens</div>
                  </div>
                ))}
              </div>
            </div>
          ) : null}
        </div>
      </DialogContent>
    </Dialog>
  );
};
