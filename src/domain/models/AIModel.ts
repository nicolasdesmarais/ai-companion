export interface AIModel {
  id: string;
  name: string;
  externalModelId: string;
  contextSize: number;
  options: AIModelOptions;
  isVisible: boolean;
  provider: AIModelProvider;
  additionalData?: any;
}

export interface AIModelOptions {
  temperature: RangeOption;
  topP: RangeOption;
  topK?: RangeOption;
  maxTokens: RangeOption;
  frequencyPenalty?: RangeOption;
  presencePenalty?: RangeOption;
}

export interface RangeOption {
  default: number;
  max: number;
  min: number;
  step: number;
}

export enum AIModelProvider {
  ANTHROPIC = "anthropic",
  AZURE_OPENAI = "azure-openai",
  AZURE_OPENAI_ASSISTANTS = "azure-openai-assistants",
  COHERE = "cohere",
  OPENAI = "openai",
  REPLICATE = "replicate",
}
