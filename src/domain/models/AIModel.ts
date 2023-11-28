export interface AIModel {
  id: string;
  name: string;
  externalModelId: string;
  contextSize: number;
  options: AIModelOptions;
  isVisible: boolean;
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
