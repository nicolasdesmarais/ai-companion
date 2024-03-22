import apiDataSourceAdapter from "@/src/adapter-out/knowledge/api/ApiDataSourceAdapter";
import fileUploadDataSourceAdapter from "@/src/adapter-out/knowledge/file-upload/FileUploadDataSourceAdapter";
import googleDriveDataSourceAdapter from "@/src/adapter-out/knowledge/google-drive/GoogleDriveDataSourceAdapter";
import msftDataSourceAdapter from "@/src/adapter-out/knowledge/msft/MsftDataSourceAdapter";
import {
  ContentRetrievingDataSourceAdapter,
  DataSourceAdapter,
} from "@/src/adapter-out/knowledge/types/DataSourceAdapter";
import webUrlsDataSourceAdapter from "@/src/adapter-out/knowledge/web-urls/WebUrlsDataSourceAdapter";
import webUrlsWebsiteContentCrawlerAdapter from "@/src/adapter-out/knowledge/web-urls/WebUrlsWebsiteContentCrawlerAdapter";
import prismadb from "@/src/lib/prismadb";
import { DataSource, DataSourceType } from "@prisma/client";
import { EntityNotFoundError } from "../errors/Errors";

const useCheerioAdapter = process.env.USE_CHEERIO_ADAPTER === "true";

export class DataSourceAdapterService {
  /**
   * Returns the DataSource and DataSourceAdapter for the given dataSourceId
   * Throws an EntityNotFoundError if the DataSource is not found
   */
  public async getDataSourceAndAdapter(dataSourceId: string): Promise<{
    dataSource: DataSource;
    dataSourceAdapter: DataSourceAdapter;
  }> {
    const dataSource = await prismadb.dataSource.findUnique({
      where: { id: dataSourceId },
    });
    if (!dataSource) {
      throw new EntityNotFoundError(
        `DataSource with id=${dataSourceId} not found`
      );
    }

    const dataSourceAdapter = this.getDataSourceAdapter(dataSource.type);
    return { dataSource, dataSourceAdapter };
  }

  public getDataSourceAdapter(type: DataSourceType): DataSourceAdapter {
    switch (type) {
      case DataSourceType.GOOGLE_DRIVE:
        return googleDriveDataSourceAdapter;
      case DataSourceType.WEB_URL:
        return useCheerioAdapter
          ? webUrlsWebsiteContentCrawlerAdapter
          : webUrlsDataSourceAdapter;
      case DataSourceType.FILE_UPLOAD:
        return fileUploadDataSourceAdapter;
      case DataSourceType.API:
        return apiDataSourceAdapter;
      case DataSourceType.ONEDRIVE:
        return msftDataSourceAdapter;
      default:
        throw new Error(`DataSourceType ${type} not supported`);
    }
  }

  public getContentRetrievingDataSourceAdapter(
    type: DataSourceType
  ): ContentRetrievingDataSourceAdapter {
    switch (type) {
      case DataSourceType.GOOGLE_DRIVE:
        return googleDriveDataSourceAdapter;
      case DataSourceType.WEB_URL:
        return useCheerioAdapter
          ? webUrlsWebsiteContentCrawlerAdapter
          : webUrlsDataSourceAdapter;
      case DataSourceType.ONEDRIVE:
        return msftDataSourceAdapter;
      default:
        throw new Error(`DataSourceType ${type} not supported`);
    }
  }
}

const dataSourceAdapterService = new DataSourceAdapterService();
export default dataSourceAdapterService;
