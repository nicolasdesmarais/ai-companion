import { AIModelRepository } from "@/src/domain/ports/AIModelRepository";
import { AIModel } from "../../domain/models/AIModel";

export class StaticAIModelRepository implements AIModelRepository {
  private models: AIModel[] = [
    {
      id: "gpt-4-assistant",
      name: "GPT-4 (32K Context) w/ Assistant",
      contextSize: 32768,
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
      id: "gpt-4",
      name: "GPT-4 (32K Context)",
      contextSize: 32768,
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
      contextSize: 16384,
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
  ];

  public async findAll(): Promise<AIModel[]> {
    return this.models;
  }

  public async findById(id: string): Promise<AIModel | null> {
    const model = this.models.find((model) => model.id === id);
    return model || null;
  }
}
