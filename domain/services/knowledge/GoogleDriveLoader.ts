import { EntityNotFoundError } from "@/domain/errors/Errors";
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

const OAUTH2_CLIENT = new google.auth.OAuth2(
  process.env.GOOGLE_CLIENT_ID,
  process.env.GOOGLE_CLIENT_SECRET,
  process.env.GOOGLE_CALLBACK_URL
);

const DRIVE_CLIENT = google.drive({ version: "v3", auth: OAUTH2_CLIENT });

export class GoogleDriveLoader {
  private async setOAuthCredentials(userId: string) {
    const oauthToken = await prismadb.oAuthToken.findUnique({
      where: {
        provider_userId: {
          userId,
          provider: "GOOGLE",
        },
      },
    });

    if (!oauthToken?.data) {
      throw new EntityNotFoundError("OAuth token not found");
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
    return await DRIVE_CLIENT.files.list({ q: query });
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

  public async loadFolder(userId: string, folderName: string) {
    this.setOAuthCredentials(userId);

    const findFolderQuery = `name = '${folderName}' and mimeType = 'application/vnd.google-apps.folder'`;
    const folderResponse = await this.listFiles(findFolderQuery);

    const folders = folderResponse.data.files;
    if (!folders || folders.length === 0) {
      throw new EntityNotFoundError("Folder not found");
    }

    const mimeTypeQuery = SUPPORTED_MIME_TYPES.map(
      (type) => `mimeType='${type}'`
    ).join(" or ");

    const folderIds = folders.map((folder) => folder.id);
    for (const folderId of folderIds) {
      const query = `'${folderId}' in parents and (${mimeTypeQuery}) and trashed = false`;
      const response = await this.listFiles(query);
      const files = response.data.files ?? [];

      files.forEach((file) => {
        this.loadFile(userId, file);
      });

      const fileNames = files.map((file) => file.name);
      return fileNames;
    }
  }

  private async loadFile(userId: string, file: drive_v3.Schema$File) {
    if (!file?.id || !file?.name || !file?.mimeType) {
      throw new EntityNotFoundError(`File not found`);
    }

    const mimeType = file.mimeType;
    const fileName = file.name;
    console.log(
      "Loading file: ",
      fileName,
      " with mime type: ",
      mimeType,
      " and id: ",
      file.id,
      " from google drive"
    );

    const fileResponse = await this.getFileAsStream(file.id);

    const filePath = `/tmp/${file.name}`;
    const writableStream = fs.createWriteStream(filePath);

    if (fileResponse.data instanceof Readable) {
      fileResponse.data.pipe(writableStream).on("finish", async () => {
        console.log(`File downloaded to ${filePath}`);

        const fileLoader = new FileLoader();
        await fileLoader.loadFileFromPath(userId, mimeType, fileName, filePath);
      });
    }
  }
}
