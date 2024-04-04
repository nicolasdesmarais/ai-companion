import { AnthropicModel } from "@/src/adapter-out/ai-model/chat-models/AnthropicModel";
import { AssistantChatModel } from "@/src/adapter-out/ai-model/chat-models/AssistantChatModel";
import { AzureOpenAIModel } from "@/src/adapter-out/ai-model/chat-models/AzureOpenAIModel";
import { ChatModel } from "@/src/adapter-out/ai-model/chat-models/ChatModel";
import { CohereModel } from "@/src/adapter-out/ai-model/chat-models/CohereModel";
import { GptAssistantModel } from "@/src/adapter-out/ai-model/chat-models/GptAssistantModel";
import { ReplicateModel } from "@/src/adapter-out/ai-model/chat-models/ReplicateModel";
import { StaticAIModelRepository } from "@/src/adapter-out/repositories/StaticAIModelRepository";
import { AuthorizationContext } from "@/src/security/models/AuthorizationContext";
import { SecuredAction } from "@/src/security/models/SecuredAction";
import { SecuredResourceAccessLevel } from "@/src/security/models/SecuredResourceAccessLevel";
import { SecuredResourceType } from "@/src/security/models/SecuredResourceType";
import { BaseEntitySecurityService } from "@/src/security/services/BaseEntitySecurityService";
import { EntityNotFoundError } from "../errors/Errors";
import { AIModel } from "../models/AIModel";
import { AIModelRepository } from "../ports/outgoing/AIModelRepository";

const CHAT_MODELS = [
  new AzureOpenAIModel(),
  new GptAssistantModel(),
  new ReplicateModel(),
  new AnthropicModel(),
  new CohereModel(),
];

const ASSISTANT_MODELS = [new GptAssistantModel()];

export class AIModelService {
  constructor(private aiModelRepository: AIModelRepository) {}

  public async getAIModels(
    authorizationContext: AuthorizationContext
  ): Promise<AIModel[]> {
    const hasInstanceAccess = BaseEntitySecurityService.hasPermission(
      authorizationContext,
      SecuredResourceType.AI,
      SecuredAction.READ,
      SecuredResourceAccessLevel.INSTANCE
    );

    if (hasInstanceAccess) {
      return this.aiModelRepository.findAll();
    }

    return this.aiModelRepository.findVisible();
  }

  public findAIModelById(id: string): AIModel | null {
    return this.aiModelRepository.findById(id);
  }

  public getAIModelById(id: string): AIModel {
    const model = this.aiModelRepository.findById(id);
    if (!model) {
      throw new EntityNotFoundError(`AIModel with id ${id} not found`);
    }
    return model;
  }

  public getChatModelInstance(model: AIModel): ChatModel | null {
    return CHAT_MODELS.find((chatModel) => chatModel.supports(model)) ?? null;
  }

  public getAssistantModelInstance(modelId: string): AssistantChatModel | null {
    const model = this.aiModelRepository.findById(modelId);
    if (!model) {
      return null;
    }

    return (
      ASSISTANT_MODELS.find((assistantModel) =>
        assistantModel.supports(model)
      ) ?? null
    );
  }
}

const aiModelRepository = new StaticAIModelRepository();
const aiModelService = new AIModelService(aiModelRepository);
export default aiModelService;
