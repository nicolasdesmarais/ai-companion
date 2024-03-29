import { inngest } from "./client";

export const onFunctionFailed = inngest.createFunction(
  {
    name: "Log Failed Function events",
    id: "log-failed-function-events",
  },
  { event: "inngest/function.failed" },
  async ({ event, step }) => {
    console.error(
      `The inngest function with functionId=${event.data.function_id} failed with the error=${event.data.error.message}`
    );
  }
);
