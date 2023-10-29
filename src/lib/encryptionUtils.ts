import crypto from "crypto";

const ENCRYPTION_KEY = process.env.APPDIRECT_ENCRYPTION_KEY;
const algorithm = "aes-256-cbc";

const iv = "586f44596e993773";

export function encrypt(text: string): string {
  if (!ENCRYPTION_KEY) {
    throw new Error("ENCRYPTION_KEY is not set");
  }

  const cipher = crypto.createCipheriv(algorithm, ENCRYPTION_KEY, iv);

  // encrypt the message
  // input encoding
  // output encoding
  let encryptedData = cipher.update(text, "utf-8", "hex");
  encryptedData += cipher.final("hex");

  return encryptedData;
}

export function decrypt(text: string): string {
  if (!ENCRYPTION_KEY) {
    throw new Error("ENCRYPTION_KEY is not set");
  }

  const decipher = crypto.createDecipheriv(algorithm, ENCRYPTION_KEY, iv);

  let decryptedData = decipher.update(text, "hex", "utf-8");

  decryptedData += decipher.final("utf8");
  return decryptedData;
}
