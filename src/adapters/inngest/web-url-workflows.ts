import { DomainEvent } from "@/src/domain/events/domain-event";
import dataSourceService from "@/src/domain/services/DataSourceService";
import webUrlsDataSourceAdapter from "../knowledge/web-urls/WebUrlsDataSourceAdapter";
import { inngest } from "./client";

export const pollWebUrlDataSource = inngest.createFunction(
  { id: "poll-web-url-datasource" },
  { event: DomainEvent.WEB_URL_POLLING_REQUESTED },
  async ({ event, step }) => {
    const dataSourceId = event.data.dataSourceId;

    await step.run("get-actor-results", async () => {
      return await webUrlsDataSourceAdapter.getActorResult(dataSourceId);
    });

    await step.run("index-datasource-knowlege", async () => {
      await dataSourceService.indexDataSourceKnowledge(dataSourceId);
    });
  }
);
