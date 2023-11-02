import { inngest } from "./client";

export async function publishEvent(eventType: DomainEvent, data: any) {
  await inngest.send({
    name: eventType,
    data,
  });
}
