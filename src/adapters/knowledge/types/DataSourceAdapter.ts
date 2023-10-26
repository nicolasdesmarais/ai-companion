import { Knowledge } from "@prisma/client";
import { DataSourceItemList } from "./DataSourceItemList";

export interface DataSourceAdapter {
  getDataSourceItemList(
    orgId: string,
    userId: string,
    data: any
  ): Promise<DataSourceItemList>;

  indexKnowledge(
    orgId: string,
    useId: string,
    knowledge: Knowledge,
    data: any
  ): Promise<void>;
}
