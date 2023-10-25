import {
  EntityNotFoundError,
  UnauthorizedError,
} from "@/src/domain/errors/Errors";
import {
  GoogleDriveSearchResponse,
  mapMimeTypeToEnum,
} from "@/src/domain/types/GoogleDriveSearchResponse";
import prismadb from "@/src/lib/prismadb";
import { put } from "@vercel/blob";
import fs from "fs";
import { drive_v3, google } from "googleapis";
import { Readable } from "stream";
import { FileLoader } from "../../../domain/services/knowledge/FileLoader";
import { DataStoreAdapter } from "../types/DataStoreAdapter";
import { DataStoreItem, DataStoreItemList } from "../types/DataStoreItemList";
import { GoogleDriveDataStoreInput } from "./types/GoogleDriveDataStoreInput";
import { GoogleDriveFileMetaData } from "./types/GoogleDriveFileMetaData";

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

export class GoogleDriveDataStoreAdapter implements DataStoreAdapter {
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

    const oauthTokenData = oauthToken.data as {
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

  public async getDataStoreItemList(orgId: string, userId: string, data: any) {
    const input = data as GoogleDriveDataStoreInput;

    await this.setOAuthCredentials(userId, input.oauthTokenId);

    const listFilesResponse = await this.listAllFiles(input.fileId);
    if (!listFilesResponse?.files || listFilesResponse.files.length === 0) {
      throw new EntityNotFoundError("Files not found");
    }

    const items: DataStoreItem[] = [];
    const result: DataStoreItemList = {
      dataStoreName: listFilesResponse.rootName,
      items,
    };
    for (const file of listFilesResponse.files) {
      const metadata: GoogleDriveFileMetaData = {
        fileId: file.id ?? "",
        mimeType: file.mimeType ?? "",
      };
      const item: DataStoreItem = {
        name: file.name ?? "",
        type: "FILE",
        metadata,
      };
      items.push(item);
    }

    return result;
  }

  public async createKnowledges(
    userId: string,
    oauthTokenId: string,
    fileId: string
  ) {
    await this.setOAuthCredentials(userId, oauthTokenId);

    const listFilesResponse = await this.listAllFiles(fileId);
    if (!listFilesResponse?.files || listFilesResponse.files.length === 0) {
      throw new EntityNotFoundError("Files not found");
    }

    const knowledgeIds: string[] = [];
    for (const fileId of listFilesResponse.files) {
      const knowledgeId = await this.loadFile(userId, fileId);
      if (knowledgeId) {
        knowledgeIds.push(knowledgeId);
      }
    }
    return knowledgeIds;
  }

  private async loadFile(userId: string, file: drive_v3.Schema$File) {
    if (!file?.id || !file?.name || !file?.mimeType) {
      throw new EntityNotFoundError(`File not found`);
    }

    const mimeType = file.mimeType;
    const fileName = file.name;

    const blobStream = await this.getFileAsStream(file.id);
    const blob = await put(fileName, blobStream.data, {
      access: "public",
    });

    const fileResponse = await this.getFileAsStream(file.id);

    const filePath = `/tmp/${file.name}`;
    const writableStream = fs.createWriteStream(filePath);

    return new Promise<string>(async (resolve, reject) => {
      if (fileResponse.data instanceof Readable) {
        fileResponse.data
          .pipe(writableStream)
          .on("finish", async () => {
            try {
              const fileLoader = new FileLoader();
              const knowledge = await fileLoader.loadFileFromPath(
                userId,
                mimeType,
                fileName,
                filePath,
                blob.url
              );
              resolve(knowledge.id);
            } catch (error) {
              reject(error);
            }
          })
          .on("error", (error) => {
            reject(error);
          });
      } else {
        resolve("");
      }
    });
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
}

const googleDriveDataStoreAdapter = new GoogleDriveDataStoreAdapter();
export default googleDriveDataStoreAdapter;
