import { put } from "@vercel/blob";
import stream from "stream";

const VERCEL_BLOB_ACCESS = "public";

export class FileStorageService {
  /**
   * Retrieves a file from the file storage service
   * @param url
   * @returns
   */
  public static async get(url: string): Promise<Blob> {
    const fetchResponse = await fetch(url);
    return await fetchResponse.blob();
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
      | FormData
      | ReadableStream<any>
      | File
  ): Promise<string> {
    const blob = await put(filename, body, {
      access: VERCEL_BLOB_ACCESS,
    });

    return blob.url;
  }
}
