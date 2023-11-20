import { DomainEvent } from "@/src/domain/events/domain-event";
import { inngest } from "./client";

export async function publishEvent(eventType: DomainEvent, data: any) {
  return await inngest.send({
    name: eventType,
    data,
  });
}
