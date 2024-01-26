import { AIModelRepository } from "@/src/domain/ports/outgoing/AIModelRepository";
import { AIModel, AIModelProvider } from "../../domain/models/AIModel";

const commonOptions = {
  temperature: {
    default: 1,
    max: 2,
    min: 0,
    step: 0.1,
  },
  topP: {
    default: 1,
    max: 1,
    min: 0,
    step: 0.01,
  },
  maxTokens: {
    default: 4000,
    max: 6000,
    min: 100,
    step: 1,
  },
  frequencyPenalty: {
    default: 0,
    max: 1,
    min: -1,
    step: 0.1,
  },
  presencePenalty: {
    default: 0,
    max: 1,
    min: -1,
    step: 0.1,
  },
};

export class StaticAIModelRepository implements AIModelRepository {
  private models: AIModel[] = [
    {
      id: "gpt-4",
      name: "GPT-4 (32K Context)",
      externalModelId: "gpt-4",
      contextSize: 32768,
      options: commonOptions,
      provider: AIModelProvider.OPENAI,
      isVisible: true,
    },
    {
      id: "gpt35-16k",
      name: "GPT-3.5 (16K Context)",
      externalModelId: "gpt35-16k",
      contextSize: 16384,
      options: commonOptions,
      provider: AIModelProvider.OPENAI,
      isVisible: true,
    },
    {
      id: "gpt-4-1106-preview-assistant",
      name: "GPT-4 Turbo w/ Assistant API (Beta)",
      externalModelId: "gpt-4-1106-preview",
      contextSize: 8192,
      options: commonOptions,
      provider: AIModelProvider.OPENAI,
      isVisible: false,
    },
    {
      id: "anthropic",
      name: "Anthropic Claude",
      externalModelId: "anthropic",
      contextSize: 16384,
      options: commonOptions,
      provider: AIModelProvider.ANTHROPIC,
      isVisible: true,
    },
    {
      id: "llama-2-13b-chat",
      name: "LLAMA2 13B Chat (4K Context)",
      externalModelId: "meta/llama-2-13b-chat",
      contextSize: 4096,
      options: commonOptions,
      provider: AIModelProvider.REPLICATE,
      isVisible: false,
    },
    {
      id: "llama-2-70b-chat",
      name: "LLAMA2 70B Chat (4K Context)",
      externalModelId: "meta/llama-2-70b-chat",
      contextSize: 4096,
      options: commonOptions,
      provider: AIModelProvider.REPLICATE,
      isVisible: false,
    },
  ];

  public async findAll(): Promise<AIModel[]> {
    return this.models;
  }

  public async findVisible(): Promise<AIModel[]> {
    return this.models.filter((model) => model.isVisible);
  }

  public findById(id: string): AIModel | null {
    const model = this.models.find((model) => model.id === id);
    return model ?? null;
  }
}
