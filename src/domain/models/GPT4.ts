export class GPT4 implements AIModel {
  id: string;
  name: string;
  contextSize: number;
  options: AIModelOptions;

  private constructor() {
    this.id = "gpt-4";
    this.name = "GPT-4 (32K Context)";
    this.contextSize = 32768;
    this.options = {
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
  }

  supports(modelName: string): boolean {
    return modelName === this.id;
  }
}
