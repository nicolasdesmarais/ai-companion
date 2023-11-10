import { Inngest, GetEvents } from "inngest";

// Create a client to send and receive events
export const inngest = new Inngest({ id: "appdirect-ai" });

export type Events = GetEvents<typeof inngest>;
