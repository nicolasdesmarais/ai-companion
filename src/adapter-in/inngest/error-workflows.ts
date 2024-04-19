import { inngest } from "./client";

export const onInngestFunctionFailed = inngest.createFunction(
  {
    id: "on-inngest-function-failed",
  },
  { event: "inngest/function.failed" },
  async ({ event, step }) => {
    console.error(
      `The inngest function with eventId=${event.id}, functionId=${event.data.function_id} failed with the error=${event.data.error.message}`
    );
  }
);
