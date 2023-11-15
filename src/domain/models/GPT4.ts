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

export const models = [
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

export const imageModels = [
  {
    id: "kandinsky-21",
    name: "Kandinsky 2.1",
  },
  {
    id: "dalle-2",
    name: "DALLE 2",
  },
  {
    id: "stable-diffusion-xl",
    name: "Stable Diffusion XL",
  },
  {
    id: "latent-consistency",
    name: "Latent Consistency",
  },
];

export const voices = [
  {
    id: "en-US-JennyNeural",
    name: "Jenny",
  },
  {
    id: "en-US-GuyNeural",
    name: "Guy",
  },
  {
    id: "en-US-AriaNeural",
    name: "Aria",
  },
  {
    id: "en-US-DavisNeural",
    name: "Davis",
  },
  {
    id: "en-US-JaneNeural",
    name: "Jane",
  },
  {
    id: "en-US-JasonNeural",
    name: "Jason",
  },
  {
    id: "en-US-NancyNeural",
    name: "Nancy",
  },
  {
    id: "en-US-SaraNeural",
    name: "Sara",
  },
  {
    id: "en-US-JaneNeural",
    name: "Jane",
  },
];
