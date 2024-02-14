import { client, v1, v2 } from "@datadog/datadog-api-client";
import { inngest } from "./client";

const configuration = client.createConfiguration();
const apiInstance = new v1.EventsApi(configuration);
const logsApi = new v2.LogsApi(configuration);

export const sendFailuresToDatadog = inngest.createFunction(
  {
    name: "Send failures to Datadog",
    id: "send-failed-function-events-to-datadog",
  },
  { event: "inngest/function.failed" },
  async ({ event, step }) => {
    await step.run("send-event-to-datadog", async () => {
      const error = event.data.error;

      const logParams: v2.LogsApiSubmitLogRequest = {
        body: [
          {
            message: `The ${event.data.function_id} function failed with the error: ${error.message}`,
            service: "appdirect-ai",
            ddtags: `env:${process.env.NEXT_PUBLIC_VERCEL_ENV}`,
          },
        ],
      };
      const response = await logsApi.submitLog(logParams);

      return { message: "Event sent successfully", response };
    });
  }
);
