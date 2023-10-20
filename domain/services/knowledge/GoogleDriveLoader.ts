import { EntityNotFoundError, UnauthorizedError } from "@/domain/errors/Errors";
import {
  GoogleDriveSearchResponse,
  mapMimeTypeToEnum,
} from "@/domain/types/GoogleDriveSearchResponse";
import prismadb from "@/lib/prismadb";
import fs from "fs";
import { drive_v3, google } from "googleapis";
import { Readable } from "stream";
import { FileLoader } from "./FileLoader";

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

export class GoogleDriveLoader {
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

    const query = `(${this.getNamesQuery(
      searchTerms
    )}) and (${this.getMimeTypeQuery(true)}) and trashed = false`;

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

  public async createKnowledges(
    userId: string,
    oauthTokenId: string,
    fileId: string
  ) {
    await this.setOAuthCredentials(userId, oauthTokenId);

    const fileIds = await this.listAllFiles(fileId);
    if (!fileIds || fileIds.length === 0) {
      throw new EntityNotFoundError("Files not found");
    }

    const knowledgeIds: string[] = [];
    for (const fileId of fileIds) {
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
                filePath
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

  private async listAllFiles(fileId: string): Promise<drive_v3.Schema$File[]> {
    const result: drive_v3.Schema$File[] = [];

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
          result.push(file);
        }
      }
    };

    const initialFile = await DRIVE_CLIENT.files.get({
      fileId,
      fields: "id, name, mimeType",
    });

    if (!initialFile.data.mimeType || !initialFile.data.id) {
      return result;
    }

    if (initialFile.data.mimeType === FOLDER_MIME_TYPE) {
      await listFilesRecursive(initialFile.data.id);
    } else {
      result.push(initialFile.data);
    }

    return result;
  }
}
