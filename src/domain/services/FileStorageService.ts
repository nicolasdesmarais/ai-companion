import { del, put } from "@vercel/blob";
import stream from "stream";

const VERCEL_BLOB_ACCESS = "public";

export class FileStorageService {
  /**
   * Retrieves a file from the file storage service as a Blob
   * @param url
   * @returns
   */
  public static async getBlob(url: string): Promise<Blob> {
    const fetchResponse = await fetch(url);
    return await fetchResponse.blob();
  }

  /**
   * Retrieves a file from the file storage service as a JSON object
   */
  public static async getJson(url: string): Promise<any> {
    const fetchResponse = await fetch(url);
    return await fetchResponse.json();
  }

  /**
   * Uploads a file to the file storage service
   * @param filename
   * @param body
   * @returns the URL of the uploaded file
   */
  public static async put(
    filename: string,
    body:
      | string
      | stream.Readable
      | Blob
      | ArrayBuffer
      | ReadableStream<any>
      | File
  ): Promise<string> {
    const encodedFilename = encodeURIComponent(filename);
    const blob = await put(encodedFilename, body, {
      access: VERCEL_BLOB_ACCESS,
    });

    return blob.url;
  }

  public static async delete(url: string): Promise<void> {
    if (!url) {
      return;
    }

    await del(url);
  }
}
