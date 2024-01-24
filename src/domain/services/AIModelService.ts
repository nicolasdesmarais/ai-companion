import { AssistantChatModel } from "@/src/adapter-out/ai-model/chat-models/AssistantChatModel";
import { ChatModel } from "@/src/adapter-out/ai-model/chat-models/ChatModel";
import { CohereModel } from "@/src/adapter-out/ai-model/chat-models/CohereModel";
import { DavinciModel } from "@/src/adapter-out/ai-model/chat-models/DavinciModel";
import { Gpt35Model } from "@/src/adapter-out/ai-model/chat-models/Gpt35Model";
import { Gpt4Model } from "@/src/adapter-out/ai-model/chat-models/Gpt4Model";
import { GptAssistantModel } from "@/src/adapter-out/ai-model/chat-models/GptAssistantModel";
import { LLamaModel } from "@/src/adapter-out/ai-model/chat-models/LLamaModel";
import { StaticAIModelRepository } from "@/src/adapter-out/repositories/StaticAIModelRepository";
import { AIModel } from "../models/AIModel";
import { AIModelRepository } from "../ports/outgoing/AIModelRepository";

const CHAT_MODELS = [
  new Gpt4Model(),
  new Gpt35Model(),
  new GptAssistantModel(),
  new DavinciModel(),
  new LLamaModel(),
  new CohereModel(),
];

const ASSISTANT_MODELS = [new GptAssistantModel()];

export class AIModelService {
  constructor(private aiModelRepository: AIModelRepository) {}

  public async getAIModels(): Promise<AIModel[]> {
    return this.aiModelRepository.findAll();
  }

  public findAIModelById(id: string): AIModel | null {
    return this.aiModelRepository.findById(id);
  }

  public getChatModelInstance(modelId: string): ChatModel | null {
    return (
      CHAT_MODELS.find((assistantModel) => assistantModel.supports(modelId)) ??
      null
    );
  }

  public getAssistantModelInstance(modelId: string): AssistantChatModel | null {
    return (
      ASSISTANT_MODELS.find((assistantModel) =>
        assistantModel.supports(modelId)
      ) ?? null
    );
  }
}

const aiModelRepository = new StaticAIModelRepository();
const aiModelService = new AIModelService(aiModelRepository);
export default aiModelService;
