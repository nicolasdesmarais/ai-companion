import {
  EntityNotFoundError,
  UnauthorizedError,
} from "@/src/domain/errors/Errors";
import {
  GoogleDriveSearchResponse,
  mapMimeTypeToEnum,
} from "@/src/domain/types/GoogleDriveSearchResponse";
import { EncryptionService } from "@/src/lib/encryptionUtils";
import prismadb from "@/src/lib/prismadb";
import { Knowledge, KnowledgeIndexStatus } from "@prisma/client";
import { put } from "@vercel/blob";
import fs from "fs";
import { drive_v3, google } from "googleapis";
import { Readable } from "stream";
import fileLoader from "../knowledgeLoaders/FileLoader";
import { DataSourceAdapter } from "../types/DataSourceAdapter";
import {
  DataSourceItem,
  DataSourceItemList,
} from "../types/DataSourceItemList";
import { IndexKnowledgeResponse } from "../types/IndexKnowledgeResponse";
import { GoogleDriveDataSourceInput } from "./types/GoogleDriveDataSourceInput";
import { GoogleDriveFileMetadata } from "./types/GoogleDriveFileMetaData";

const SUPPORTED_MIME_TYPES = [
  "text/plain",
  "text/csv",
  "application/epub+zip",
  "application/pdf",
  "application/vnd.openxmlformats-officedocument.wordprocessingml.document",
];

const FOLDER_MIME_TYPE = "application/vnd.google-apps.folder";

const OAUTH2_CLIENT = new google.auth.OAuth2(
  process.env.GOOGLE_CLIENT_ID,
  process.env.GOOGLE_CLIENT_SECRET,
  process.env.GOOGLE_CALLBACK_URL
);

const DRIVE_CLIENT = google.drive({ version: "v3", auth: OAUTH2_CLIENT });

interface ListFilesResponse {
  rootName: string;
  files: drive_v3.Schema$File[];
}

export class GoogleDriveDataSourceAdapter implements DataSourceAdapter {
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

  private async setOAuthCredentials(userId: string, oauthTokenId: string) {
    const oauthToken = await prismadb.oAuthToken.findUnique({
      where: {
        id: oauthTokenId,
      },
    });

    if (!oauthToken?.data) {
      throw new EntityNotFoundError("OAuth token not found");
    }

    if (oauthToken.userId !== userId) {
      throw new UnauthorizedError("Unauthorized access to OAuth token");
    }

    const encryptionService = new EncryptionService();
    const oauthTokenData = JSON.parse(
      encryptionService.decrypt(oauthToken.data)
    ) as {
      access_token: string;
      refresh_token: string;
    };

    OAUTH2_CLIENT.setCredentials({
      access_token: oauthTokenData.access_token,
      refresh_token: oauthTokenData.refresh_token,
    });
  }

  private async listFiles(query: string) {
    return await DRIVE_CLIENT.files.list({
      q: query,
      fields: "files(id, name, mimeType, owners, modifiedTime)",
    });
  }

  private async getFileAsStream(fileId: string) {
    return await DRIVE_CLIENT.files.get(
      {
        fileId: fileId,
        alt: "media",
      },
      { responseType: "stream" }
    );
  }

  public async search(
    userId: string,
    oauthTokenId: string,
    searchTerms: string[]
  ) {
    await this.setOAuthCredentials(userId, oauthTokenId);

    let query;
    if (searchTerms.length > 0) {
      query = `(${this.getNamesQuery(
        searchTerms
      )}) and (${this.getMimeTypeQuery(true)}) and trashed = false`;
    } else {
      query = `(${this.getMimeTypeQuery(true)}) and trashed = false`;
    }

    const googleDriveSearchResponse = await this.listFiles(query);
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

  public async getDataSourceItemList(orgId: string, userId: string, data: any) {
    const input = data as GoogleDriveDataSourceInput;

    await this.setOAuthCredentials(userId, input.oauthTokenId);

    const listFilesResponse = await this.listAllFiles(input.fileId);
    if (!listFilesResponse?.files || listFilesResponse.files.length === 0) {
      return {
        dataSourceName: listFilesResponse.rootName,
        items: [],
      };
    }

    const items: DataSourceItem[] = [];
    const result: DataSourceItemList = {
      dataSourceName: listFilesResponse.rootName,
      items,
    };
    for (const file of listFilesResponse.files) {
      const metadata: GoogleDriveFileMetadata = {
        fileId: file.id ?? "",
        fileName: file.name ?? "",
        mimeType: file.mimeType ?? "",
      };
      const item: DataSourceItem = {
        name: file.name ?? "",
        type: "FILE",
        metadata,
      };
      items.push(item);
    }

    return result;
  }

  public async indexKnowledge(
    orgId: string,
    userId: string,
    knowledge: Knowledge,
    data: any
  ): Promise<IndexKnowledgeResponse> {
    const input = data as GoogleDriveDataSourceInput;
    await this.setOAuthCredentials(userId, data.oauthTokenId);

    if (!knowledge.metadata) {
      throw new Error("Knowledge metadata not found for indexing Google Drive");
    }

    const { fileId, fileName, mimeType } =
      knowledge.metadata as unknown as GoogleDriveFileMetadata;

    const blobStream = await this.getFileAsStream(fileId);
    const blob = await put(fileName, blobStream.data, {
      access: "public",
    });
    knowledge.blobUrl = blob.url;

    const fileResponse = await this.getFileAsStream(fileId);
    const filePath = `/tmp/${fileName}`;
    const writableStream = fs.createWriteStream(filePath);

    if (fileResponse.data instanceof Readable) {
      fileResponse.data.pipe(writableStream).on("finish", async () => {
        try {
          await fileLoader.loadFile(knowledge.id, fileName, mimeType, filePath);
        } catch (error) {
          console.log(error);
          throw new Error("Failed to load file from google drive");
        }
      });
    }

    return {
      indexStatus: KnowledgeIndexStatus.COMPLETED,
    };
  }

  private async listAllFiles(fileId: string): Promise<ListFilesResponse> {
    const files: drive_v3.Schema$File[] = [];

    const listFilesRecursive = async (folderId: string): Promise<void> => {
      const query = `'${folderId}' in parents and (${this.getMimeTypeQuery(
        true
      )}) and trashed=false`;

      const response = await this.listFiles(query);

      if (!response.data.files) return;

      for (const file of response.data.files) {
        if (file.mimeType === FOLDER_MIME_TYPE) {
          await listFilesRecursive(file.id!);
        } else {
          files.push(file);
        }
      }
    };

    const initialFile = await DRIVE_CLIENT.files.get({
      fileId,
      fields: "id, name, mimeType",
    });
    const rootName = initialFile.data.name ?? "";

    if (!initialFile.data.mimeType || !initialFile.data.id) {
      return {
        rootName,
        files,
      };
    }

    if (initialFile.data.mimeType === FOLDER_MIME_TYPE) {
      await listFilesRecursive(initialFile.data.id);
    } else {
      files.push(initialFile.data);
    }

    return {
      rootName,
      files,
    };
  }

  public retrieveKnowledgeIdFromEvent(data: any): string {
    throw new Error("Method not implemented.");
  }

  public async handleKnowledgeIndexedEvent(
    knowledge: Knowledge,
    data: any
  ): Promise<IndexKnowledgeResponse> {
    throw new Error("Method not implemented.");
  }

  public async deleteKnowledge(knowledgeId: string): Promise<void> {
    fileLoader.deleteKnowledge(knowledgeId);
  }
}

const googleDriveDataSourceAdapter = new GoogleDriveDataSourceAdapter();
export default googleDriveDataSourceAdapter;
