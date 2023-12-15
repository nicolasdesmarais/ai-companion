import { AIModelRepository } from "@/src/domain/ports/outgoing/AIModelRepository";
import { AIModel } from "../../domain/models/AIModel";

const SHOW_ALL_AI_MODELS = process.env.SHOW_ALL_AI_MODELS !== "false";

export class StaticAIModelRepository implements AIModelRepository {
  private models: AIModel[] = [
    {
      id: "gpt-4",
      name: "GPT-4 (32K Context)",
      externalModelId: "gpt-4",
      contextSize: 32768,
      isVisible: true,
      options: {
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
      },
    },
    {
      id: "gpt35-16k",
      name: "GPT-3.5 (16K Context)",
      externalModelId: "gpt35-16k",
      contextSize: 16384,
      isVisible: true,
      options: {
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
      },
    },
    {
      id: "gpt-4-1106-preview-assistant",
      name: "GPT-4 Turbo w/ Assistant API (Beta)",
      externalModelId: "gpt-4-1106-preview",
      contextSize: 8192,
      isVisible: false,
      options: {
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
      },
    },
    // {
    //   id: "llama2-13b",
    //   name: "LLAMA2 13B Chat (4K Context)",
    //   contextSize: 4096,
    //   options: {
    //     temperature: {
    //       default: 0.75,
    //       max: 2,
    //       min: 0,
    //       step: 0.1,
    //     },
    //     topP: {
    //       default: 0.9,
    //       max: 1,
    //       min: 0,
    //       step: 0.01,
    //     },
    //     topK: {
    //       default: 50,
    //       max: 100,
    //       min: 0,
    //       step: 1,
    //     },
    //     maxTokens: {
    //       default: 1275,
    //       max: 2000,
    //       min: 1275,
    //       step: 1,
    //     },
    //   },
    // },
    // {
    //   id: "text-davinci-003",
    //   name: "DaVinci-003 (4K Context)",
    //   contextSize: 4096,
    //   options: {
    //     temperature: {
    //       default: 1,
    //       max: 2,
    //       min: 0,
    //       step: 0.1,
    //     },
    //     topP: {
    //       default: 1,
    //       max: 1,
    //       min: 0,
    //       step: 0.01,
    //     },
    //     maxTokens: {
    //       default: 600,
    //       max: 2000,
    //       min: 500,
    //       step: 1,
    //     },
    //     frequencyPenalty: {
    //       default: 0,
    //       max: 2,
    //       min: -2,
    //       step: 0.1,
    //     },
    //     presencePenalty: {
    //       default: 0,
    //       max: 2,
    //       min: -2,
    //       step: 0.1,
    //     },
    //   },
    // },
  ];

  public async findAll(): Promise<AIModel[]> {
    if (SHOW_ALL_AI_MODELS) {
      return this.models;
    }

    return this.models.filter((model) => model.isVisible);
  }

  public findById(id: string): AIModel | null {
    const model = this.models.find((model) => model.id === id);
    return model ?? null;
  }
}
