export enum ApifySupportedEvents {
  ACTOR_RUN_SUCCEEDED = "ACTOR.RUN.SUCCEEDED",
  ACTOR_RUN_FAILED = "ACTOR.RUN.FAILED",
  ACTOR_RUN_ABORTED = "ACTOR.RUN.ABORTED",
  ACTOR_RUN_TIMED_OUT = "ACTOR.RUN.TIMED_OUT",
}

export interface ApifyWebhookEvent {
  eventType: string;
  eventData: ApifyEventData;
  knowledgeId: string;
}

interface ApifyEventData {
  actorId: string;
  actorRunId: string;
}
