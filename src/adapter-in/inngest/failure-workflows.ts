import { client, v1 } from "@datadog/datadog-api-client";
import { inngest } from "./client";

const configuration = client.createConfiguration();
const apiInstance = new v1.EventsApi(configuration);

export default inngest.createFunction(
  {
    name: "Send failures to Datadog",
    id: "send-failed-function-events-to-datadog",
  },
  { event: "inngest/function.failed" },
  async ({ event, step }) => {
    await step.run("send-event-to-datadog", async () => {
      const error = event.data.error;

      const params: v1.EventsApiCreateEventRequest = {
        body: {
          title: "Inngest Function Failed",
          alertType: "error",
          text: `The ${event.data.function_id} function failed with the error: ${error.message}`,
          tags: [`inngest_function_id:${event.data.function_id}`],
        },
      };

      const data = await apiInstance.createEvent(params);

      return { message: "Event sent successfully", data };
    });
  }
);
