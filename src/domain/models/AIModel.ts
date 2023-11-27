export interface AIModel {
  id: string;
  name: string;
  contextSize: number;
  options: {
    temperature: RangeOption;
    topP: RangeOption;
    topK?: RangeOption;
    maxTokens: RangeOption;
    frequencyPenalty?: RangeOption;
    presencePenalty?: RangeOption;
  };
}

export interface RangeOption {
  default: number;
  max: number;
  min: number;
  step: number;
}
