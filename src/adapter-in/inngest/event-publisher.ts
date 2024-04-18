import { inngest } from "./client";

export async function publishEvent(eventType: any, data: any) {
  return await inngest.send({
    name: eventType,
    data,
  });
}
