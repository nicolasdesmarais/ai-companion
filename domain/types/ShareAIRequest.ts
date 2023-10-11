import { AIVisibility } from "@prisma/client";

export interface ShareAIRequest {
  visibility: AIVisibility;
  emails: string[];
}
