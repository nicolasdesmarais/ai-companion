import {
  GoogleDriveSearchResponse,
  mapMimeTypeToEnum,
} from "@/src/adapter-in/api/GoogleDriveApi";
import {
  EntityNotFoundError,
  ForbiddenError,
} from "@/src/domain/errors/Errors";
import { DomainEvent } from "@/src/domain/events/domain-event";
import orgClientCredentialsService from "@/src/domain/services/OrgClientCredentialsService";
import { decryptFromBuffer } from "@/src/lib/encryptionUtils";
import prismadb from "@/src/lib/prismadb";
import {
  DataSourceType,
  Knowledge,
  KnowledgeIndexStatus,
  OAuthTokenProvider,
} from "@prisma/client";
import { put } from "@vercel/blob";
import { GaxiosResponse } from "gaxios";
import { drive_v3 } from "googleapis";
import { Readable } from "stream";
import { publishEvent } from "../../../adapter-in/inngest/event-publisher";
import {
  googleDriveClient,
  googleDriveOauth2Client,
} from "../../oauth/GoogleDriveClient";
import fileLoader from "../knowledgeLoaders/FileLoader";
import { ContentRetrievingDataSourceAdapter } from "../types/DataSourceAdapter";
import {
  DataSourceItem,
  DataSourceItemList,
  RetrieveContentResponse,
  RetrieveContentResponseStatus,
} from "../types/DataSourceTypes";
import { IndexKnowledgeResponse } from "../types/IndexKnowledgeResponse";
import {
  KnowledgeIndexingResult,
  KnowledgeIndexingResultStatus,
} from "../types/KnowlegeIndexingResult";
import { OrgAndKnowledge } from "../types/OrgAndKnowledge";
import {
  GoogleDriveEvent,
  GoogleDriveFolderScanInitiatedPayload,
} from "./events/GoogleDriveEvent";
import { GoogleDriveDataSourceInput } from "./types/GoogleDriveDataSourceInput";
import { GoogleDriveFileMetadata } from "./types/GoogleDriveFileMetaData";
import {
  FOLDER_MIME_TYPE,
  MIME_TYPE_CSV,
  MIME_TYPE_DOCX,
  MIME_TYPE_GOOGLE_DOC,
  MIME_TYPE_GOOGLE_SHEETS,
  MIME_TYPE_GOOGLE_SLIDES,
  MIME_TYPE_PDF,
  SUPPORTED_MIME_TYPES,
  mapGoogleDriveFileToDataSourceItem,
} from "./util/GoogleDriveUtils";

export class GoogleDriveDataSourceAdapter
  implements ContentRetrievingDataSourceAdapter
{
  private getNamesQuery(names: string[]) {
    return names.map((name) => `name contains '${name}'`).join(" AND ");
  }

  private getMimeTypeQuery(includeFolders: boolean) {
    const mimeTypes: string[] = [];
    mimeTypes.push(...SUPPORTED_MIME_TYPES);
    if (includeFolders) {
      mimeTypes.push(FOLDER_MIME_TYPE);
    }

    return mimeTypes.map((type) => `mimeType='${type}'`).join(" or ");
  }

  private async setOAuthCredentials(
    orgId: string,
    userId: string,
    oauthTokenId: string
  ) {
    const oauthToken = await prismadb.oAuthToken.findUnique({
      where: {
        id: oauthTokenId,
      },
    });

    if (!oauthToken?.data) {
      throw new EntityNotFoundError("OAuth token not found");
    }

    if (oauthToken.userId !== userId) {
      throw new ForbiddenError("Unauthorized access to OAuth token");
    }

    const oauthTokenData = JSON.parse(decryptFromBuffer(oauthToken.data)) as {
      access_token: string;
      refresh_token: string;
    };

    const orgClientCredentialData =
      await orgClientCredentialsService.getOrgClientCredentialData(
        orgId,
        OAuthTokenProvider.GOOGLE
      );

    const oauth2Client = googleDriveOauth2Client(orgClientCredentialData);
    oauth2Client.setCredentials({
      access_token: oauthTokenData.access_token,
      refresh_token: oauthTokenData.refresh_token,
    });

    return oauth2Client;
  }

  private async listFiles(googleDriveClient: drive_v3.Drive, query: string) {
    return await googleDriveClient.files.list({
      q: query,
      fields: "files(id, name, mimeType, owners, modifiedTime)",
      supportsAllDrives: true,
      includeItemsFromAllDrives: true,
      corpora: "allDrives",
    });
  }

  private async getFileAsStream(
    googleDriveClient: drive_v3.Drive,
    fileId: string
  ) {
    return await googleDriveClient.files.get(
      {
        fileId: fileId,
        alt: "media",
        supportsAllDrives: true,
      },
      { responseType: "stream" }
    );
  }

  private async getGoogleDocContent(
    googleDriveClient: drive_v3.Drive,
    fileId: string,
    exportedMimeType: string
  ) {
    const response = await googleDriveClient.files.export(
      {
        fileId: fileId,
        mimeType: exportedMimeType,
      },
      {
        responseType: "stream",
      }
    );

    return response;
  }

  public async search(
    orgId: string,
    userId: string,
    oauthTokenId: string,
    searchTerms: string[]
  ) {
    const oauth2Client = await this.setOAuthCredentials(
      orgId,
      userId,
      oauthTokenId
    );

    const driveClient = await googleDriveClient(oauth2Client);

    let query;
    if (searchTerms.length > 0) {
      query = `(${this.getNamesQuery(
        searchTerms
      )}) and (${this.getMimeTypeQuery(true)}) and trashed = false`;
    } else {
      query = `(${this.getMimeTypeQuery(true)}) and trashed = false`;
    }

    const googleDriveSearchResponse = await this.listFiles(driveClient, query);
    const files = googleDriveSearchResponse.data.files?.map((file) => {
      return {
        id: file.id ?? "",
        name: file.name ?? "",
        type: mapMimeTypeToEnum(file.mimeType),
        owner: file.owners?.[0]?.displayName ?? "",
        modifiedTime: file.modifiedTime ?? "",
      };
    });

    const response: GoogleDriveSearchResponse = {
      files: files ?? [],
    };
    return response;
  }

  /**
   * Retrieves a list of files from Google Drive
   * If the fileId is a file:
   *  - returns a list with a single item
   * If the fileId is a folder:
   *  - publishes a FOLDER_SCAN_INITIATED event to process the folder asynchronously
   *  - returns an empty list of items
   * @param orgId
   * @param userId
   * @param data
   * @returns
   */
  public async getDataSourceItemList(
    orgId: string,
    userId: string,
    dataSourceId: string,
    data: any,
    forRefresh: boolean,
    forceRefresh: boolean
  ): Promise<DataSourceItemList> {
    const input = data as GoogleDriveDataSourceInput;
    const oauthTokenId = input.oauthTokenId;

    const oauth2Client = await this.setOAuthCredentials(
      orgId,
      userId,
      oauthTokenId
    );
    const driveClient = googleDriveClient(oauth2Client);

    const inputFile = await driveClient.files.get({
      fileId: input.fileId,
      fields: "id, name, mimeType, modifiedTime",
      supportsAllDrives: true,
    });

    const fileData = inputFile.data;
    if (!fileData.id) {
      return {
        items: [],
      };
    }

    const dataSourceItem = await this.extractDataSourceItemFromFile(
      orgId,
      userId,
      oauthTokenId,
      dataSourceId,
      fileData,
      forRefresh,
      forceRefresh
    );
    const dataSourceItemList: DataSourceItem[] = [];
    if (dataSourceItem) {
      dataSourceItemList.push(dataSourceItem);
    }
    return {
      items: dataSourceItemList,
    };
  }

  /**
   * Retrieves a list of files from a Google Drive folder.
   * Each file in the folder is returned as a DataSourceItem, with all relevant metadata.
   * For each child folder in the folder, a FOLDER_SCAN_INITIATED event is published
   * to process the folder asynchronously.
   * @param orgId
   * @param userId
   * @param oauthTokenId
   * @param dataSourceId
   * @param folderId
   * @returns
   */
  public async getDataSourceItemListFromFolder(
    orgId: string,
    userId: string,
    oauthTokenId: string,
    dataSourceId: string,
    folderId: string,
    forRefresh: boolean,
    forceRefresh: boolean
  ): Promise<DataSourceItemList> {
    const oauth2Client = await this.setOAuthCredentials(
      orgId,
      userId,
      oauthTokenId
    );
    const driveClient = googleDriveClient(oauth2Client);

    const query = `'${folderId}' in parents and (${this.getMimeTypeQuery(
      true
    )}) and trashed=false`;

    const response = await this.listFiles(driveClient, query);

    if (!response.data.files) {
      return {
        items: [],
      };
    }

    const files: DataSourceItem[] = [];
    for (const file of response.data.files) {
      const item = await this.extractDataSourceItemFromFile(
        orgId,
        userId,
        oauthTokenId,
        dataSourceId,
        file,
        forRefresh,
        forceRefresh,
        folderId
      );
      if (item) {
        files.push(item);
      }
    }
    return {
      items: files,
    };
  }

  private async extractDataSourceItemFromFile(
    orgId: string,
    userId: string,
    oauthTokenId: string,
    dataSourceId: string,
    file: drive_v3.Schema$File,
    forRefresh: boolean,
    forceRefresh: boolean,
    parentFolderId?: string
  ): Promise<DataSourceItem | null> {
    if (!file.id) {
      return null;
    }
    // For folders, publish event to process folder asynchronously
    // Return an empty list of items
    if (file.mimeType === FOLDER_MIME_TYPE) {
      const eventPayload: GoogleDriveFolderScanInitiatedPayload = {
        orgId,
        userId,
        oauthTokenId,
        dataSourceId,
        folderId: file.id,
        forRefresh,
        forceRefresh,
      };
      await publishEvent(
        GoogleDriveEvent.GOOGLE_DRIVE_FOLDER_SCAN_INITIATED,
        eventPayload
      );
      return null;
    }

    return mapGoogleDriveFileToDataSourceItem(file, parentFolderId);
  }

  public async retrieveKnowledgeContent(
    orgId: string,
    userId: string,
    knowledge: Knowledge,
    data: any
  ): Promise<RetrieveContentResponse> {
    if (!userId) {
      console.error("Missing userId");
      return {
        status: RetrieveContentResponseStatus.FAILED,
      };
    }
    if (!data?.oauthTokenId) {
      console.error("Missing oauthTokenId");
      return {
        status: RetrieveContentResponseStatus.FAILED,
      };
    }

    const oauth2Client = await this.setOAuthCredentials(
      orgId,
      userId,
      data?.oauthTokenId
    );
    const driveClient = googleDriveClient(oauth2Client);

    const { fileId, fileName, mimeType } =
      knowledge.metadata as unknown as GoogleDriveFileMetadata;

    const { fileResponse } = await this.getFileContent(
      driveClient,
      fileId,
      mimeType
    );

    const buffer = await this.streamToBuffer(fileResponse.data);
    const blob = await put(fileName, buffer, {
      access: "public",
    });

    return {
      status: RetrieveContentResponseStatus.SUCCESS,
      contentBlobUrl: blob.url,
    };
  }

  public async indexKnowledge(
    orgId: string,
    userId: string,
    knowledge: Knowledge,
    data: any
  ): Promise<IndexKnowledgeResponse> {
    if (knowledge.indexStatus === KnowledgeIndexStatus.COMPLETED) {
      return {
        indexStatus: KnowledgeIndexStatus.COMPLETED,
      };
    }
    if (!userId) {
      console.error("Missing userId");
      return {
        indexStatus: KnowledgeIndexStatus.FAILED,
      };
    }
    if (!data?.oauthTokenId) {
      console.error("Missing oauthTokenId");
      return {
        indexStatus: KnowledgeIndexStatus.FAILED,
      };
    }

    const oauth2Client = await this.setOAuthCredentials(
      orgId,
      userId,
      data?.oauthTokenId
    );
    const driveClient = googleDriveClient(oauth2Client);

    const { fileId, fileName, mimeType } =
      knowledge.metadata as unknown as GoogleDriveFileMetadata;

    const { fileResponse, derivedMimeType } = await this.getFileContent(
      driveClient,
      fileId,
      mimeType
    );

    return new Promise((resolve) => {
      if (fileResponse.data instanceof Readable) {
        const chunks: any[] = [];
        fileResponse.data.on("data", (chunk) => {
          chunks.push(chunk);
        });

        fileResponse.data.on("end", async () => {
          const buffer = Buffer.concat(chunks);
          const blob = new Blob([buffer]);

          const { docs, metadata } = await fileLoader.getLangchainDocs(
            knowledge.id,
            fileName,
            derivedMimeType,
            blob
          );

          const cloudBlob = await put(
            `${knowledge.name}.json`,
            JSON.stringify(docs),
            {
              access: "public",
            }
          );
          knowledge.blobUrl = cloudBlob.url;

          let eventIds: string[] = [];
          for (let i = 0; i < docs.length; i++) {
            const eventResult = await publishEvent(
              DomainEvent.KNOWLEDGE_CHUNK_RECEIVED,
              {
                orgId,
                knowledgeIndexingResult: {
                  knowledgeId: knowledge.id,
                  result: {
                    chunkCount: docs.length,
                    status: KnowledgeIndexingResultStatus.SUCCESSFUL,
                  },
                },
                dataSourceType: DataSourceType.GOOGLE_DRIVE,
                index: i,
              }
            );
            eventIds = eventIds.concat(eventResult.ids);
          }
          resolve({
            userId,
            indexStatus: KnowledgeIndexStatus.INDEXING,
            documentCount: metadata.documentCount,
            tokenCount: metadata.totalTokenCount,
            metadata: {
              eventIds,
              ...metadata,
            },
          });
        });
      }
    });
  }

  public shouldReindexKnowledge(
    knowledge: Knowledge,
    item: DataSourceItem
  ): boolean {
    const { modifiedTime } =
      knowledge.metadata as unknown as GoogleDriveFileMetadata;
    return modifiedTime !== item.metadata.modifiedTime;
  }

  private async getFileContent(
    googleDriveClient: drive_v3.Drive,
    fileId: string,
    mimeType: string
  ) {
    let fileResponse: GaxiosResponse<Readable>;
    let derivedMimeType = mimeType;
    switch (mimeType) {
      case MIME_TYPE_GOOGLE_DOC:
        fileResponse = await this.getGoogleDocContent(
          googleDriveClient,
          fileId,
          MIME_TYPE_DOCX
        );
        derivedMimeType = MIME_TYPE_DOCX;
        break;
      case MIME_TYPE_GOOGLE_SHEETS:
        fileResponse = await this.getGoogleDocContent(
          googleDriveClient,
          fileId,
          MIME_TYPE_CSV
        );
        derivedMimeType = MIME_TYPE_CSV;
        break;
      case MIME_TYPE_GOOGLE_SLIDES:
        fileResponse = await this.getGoogleDocContent(
          googleDriveClient,
          fileId,
          MIME_TYPE_PDF
        );
        derivedMimeType = MIME_TYPE_PDF;
        break;
      default:
        fileResponse = await this.getFileAsStream(googleDriveClient, fileId);
    }

    return {
      fileResponse,
      derivedMimeType,
    };
  }

  public retrieveOrgAndKnowledgeIdFromEvent(data: any): OrgAndKnowledge {
    throw new Error("Method not implemented.");
  }

  getKnowledgeResultFromEvent(
    knowledge: Knowledge,
    data: any
  ): Promise<KnowledgeIndexingResult> {
    throw new Error("Method not supported.");
  }

  public async loadKnowledgeResult(
    knowledge: Knowledge,
    result: KnowledgeIndexingResult,
    index: number
  ): Promise<IndexKnowledgeResponse> {
    if (!knowledge.blobUrl) {
      console.error("loadKnowledgeResult: blob fail");
      return {
        indexStatus: KnowledgeIndexStatus.FAILED,
      };
    }

    const response = await fetch(knowledge.blobUrl);
    if (response.status !== 200) {
      console.error("loadKnowledgeResult: fetch fail");
      return {
        indexStatus: KnowledgeIndexStatus.FAILED,
      };
    }
    const data = await response.json();
    if (!data || !data[index]) {
      console.error("loadKnowledgeResult: data fail");
      return {
        indexStatus: KnowledgeIndexStatus.FAILED,
      };
    }

    const docs = [data[index]];
    const metadata = await fileLoader.loadDocs(docs, index);

    console.log("Chunk loading", knowledge.id, index);

    let indexStatus;
    switch (result.status) {
      case KnowledgeIndexingResultStatus.SUCCESSFUL:
      case KnowledgeIndexingResultStatus.PARTIAL:
        indexStatus = KnowledgeIndexStatus.PARTIALLY_COMPLETED;
        break;
      case KnowledgeIndexingResultStatus.FAILED:
        indexStatus = KnowledgeIndexStatus.FAILED;
    }
    return {
      indexStatus,
      metadata: {
        ...metadata,
        completedChunks: [index],
      },
    };
  }

  public async pollKnowledgeIndexingStatus(
    knowledge: Knowledge
  ): Promise<IndexKnowledgeResponse> {
    return {
      indexStatus: knowledge.indexStatus ?? KnowledgeIndexStatus.INITIALIZED,
    };
  }

  public async deleteKnowledge(knowledgeId: string): Promise<void> {
    await fileLoader.deleteKnowledge(knowledgeId);
  }

  public async getRemovedKnowledgeIds(
    dataSourceItemList: DataSourceItemList
  ): Promise<string[]> {
    const parentFolderIds = new Set(
      dataSourceItemList.items
        .filter((item) => item.metadata?.parentFolderId)
        .map((item) => item.metadata.parentFolderId)
    );

    const uniqueIds = dataSourceItemList.items
      .map((item) => item.uniqueId)
      .filter((uniqueId) => uniqueId !== undefined) as string[];

    const removedKnowledgeIds: string[] = [];
    for (const folderId of Array.from(parentFolderIds)) {
      const folderRemovedKnowledgeIds = await prismadb.knowledge.findMany({
        select: {
          id: true,
        },
        where: {
          uniqueId: {
            not: {
              in: uniqueIds,
            },
          },
          metadata: {
            path: "$.parentFolderId",
            equals: folderId,
          },
        },
      });
      folderRemovedKnowledgeIds.forEach((knowledge) =>
        removedKnowledgeIds.push(knowledge.id)
      );
    }

    return removedKnowledgeIds;
  }

  private async streamToBuffer(stream: Readable): Promise<Buffer> {
    const chunks: Buffer[] = [];
    for await (const chunk of stream) {
      chunks.push(chunk instanceof Buffer ? chunk : Buffer.from(chunk));
    }
    return Buffer.concat(chunks);
  }
}

const googleDriveDataSourceAdapter = new GoogleDriveDataSourceAdapter();
export default googleDriveDataSourceAdapter;
