interface AIModel {
  id: string;
  name: string;
  contextSize: number;
  options: AIModelOptions;

  supports(modelName: string): boolean;
}

interface AIModelOptions {
  temperature: AIModelOptionProperties;
  topP: AIModelOptionProperties;
  maxTokens: AIModelOptionProperties;
  frequencyPenalty: AIModelOptionProperties;
  presencePenalty: AIModelOptionProperties;
}

interface AIModelOptionProperties {
  default: number;
  max: number;
  min: number;
  step: number;
}
