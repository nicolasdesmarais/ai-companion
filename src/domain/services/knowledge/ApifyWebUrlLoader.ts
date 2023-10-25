import { EntityNotFoundError } from "@/src/domain/errors/Errors";
import {
  ApifySupportedEvents,
  ApifyWebhookEvent,
} from "@/src/domain/types/ApifyWebhookEvent";
import prismadb from "@/src/lib/prismadb";
import { ApifyService } from "../ApifyService";
import { FileLoader } from "./FileLoader";
import { put } from "@vercel/blob";

export class ApifyWebUrlLoader {
  public async loadFromWebhook(event: ApifyWebhookEvent) {
    if (event.eventType !== ApifySupportedEvents.ACTOR_RUN_SUCCEEDED) {
      // TODO: Handle failures
      return;
    }

    const knowledge = await prismadb.knowledge.findUnique({
      where: {
        id: event.knowledgeId,
      },
    });
    if (!knowledge) {
      throw new EntityNotFoundError(
        `Knowledge with id=${event.knowledgeId} not found`
      );
    }

    const apifyService = new ApifyService();
    const result = await apifyService.getActorRunResult(
      event.eventData.actorRunId
    );
    const blob = await put(
      `${encodeURI(knowledge.name)}.json`,
      JSON.stringify(result),
      {
        access: "public",
      }
    );
    await prismadb.knowledge.update({
      where: {
        id: event.knowledgeId,
      },
      data: {
        blobUrl: blob.url,
      },
    });
    const fileLoader = new FileLoader();
    fileLoader.loadJsonArray(result, knowledge.id);
  }
}
