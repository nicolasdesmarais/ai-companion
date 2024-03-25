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
      isVisible: true,
    },
    {
      id: "anthropic",
      name: "Anthropic Claude 2.1",
      externalModelId: "claude-2.1",
      contextSize: 200000,
      options: commonOptions,
      provider: AIModelProvider.ANTHROPIC,
      isVisible: false,
    },
    {
      id: "anthropic-claude-3-opus",
      name: "Anthropic Claude 3 Opus",
      externalModelId: "claude-3-opus-20240229",
      contextSize: 200000,
      options: commonOptions,
      provider: AIModelProvider.ANTHROPIC,
      isVisible: false,
    },
    {
      id: "anthropic-claude-3-sonnet",
      name: "Anthropic Claude 3 Sonnet",
      externalModelId: "claude-3-sonnet-20240229",
      contextSize: 200000,
      options: commonOptions,
      provider: AIModelProvider.ANTHROPIC,
      isVisible: false,
    },
    {
      id: "llama-2-13b-chat",
      name: "LLAMA2 13B Chat (4K Context)",
      externalModelId: "llama-2-13b-chat",
      contextSize: 4096,
      options: commonOptions,
      provider: AIModelProvider.REPLICATE,
      isVisible: false,
      additionalData: {
        owner: "meta",
        version:
          "f4e2de70d66816a838a89eeeb621910adffb0dd0baba3976c96980970978018d",
      },
    },
    {
      id: "llama-2-70b-chat",
      name: "LLAMA2 70B Chat (4K Context)",
      externalModelId: "llama-2-70b-chat",
      contextSize: 4096,
      options: {
        ...commonOptions,
        maxTokens: {
          default: 1000,
          max: 3500,
          min: 1,
          step: 1,
        },
      },
      provider: AIModelProvider.REPLICATE,
      isVisible: false,
      additionalData: {
        owner: "meta",
        version:
          "02e509c789964a7ea8736978a43525956ef40397be9033abf9fd2badfe68c9e3",
      },
    },
    {
      id: "llava-13b",
      name: "Llava 13b",
      externalModelId: "llava-13b",
      contextSize: 4096,
      options: {
        ...commonOptions,
        maxTokens: {
          default: 1000,
          max: 3500,
          min: 1,
          step: 1,
        },
      },
      provider: AIModelProvider.REPLICATE,
      isVisible: false,
      additionalData: {
        owner: "yorickvp",
        version:
          "e272157381e2a3bf12df3a8edd1f38d1dbd736bbb7437277c8b34175f8fce358",
      },
    },
    {
      id: "cohere",
      name: "Cohere command",
      externalModelId: "command",
      contextSize: 4096,
      options: {
        ...commonOptions,
        maxTokens: {
          default: 1000,
          max: 3500,
          min: 1,
          step: 1,
        },
      },
      provider: AIModelProvider.COHERE,
      isVisible: false,
    },
    {
      id: "cohere-command-light",
      name: "Cohere command-light",
      externalModelId: "command-light",
      contextSize: 4096,
      options: {
        ...commonOptions,
        maxTokens: {
          default: 1000,
          max: 3500,
          min: 1,
          step: 1,
        },
      },
      provider: AIModelProvider.COHERE,
      isVisible: false,
    },
    {
      id: "cohere-command-r",
      name: "Cohere command-r",
      externalModelId: "command-r",
      contextSize: 128000,
      options: commonOptions,
      provider: AIModelProvider.COHERE,
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
