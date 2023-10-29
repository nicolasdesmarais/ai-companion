import crypto from "crypto";

const ENCRYPTION_KEY = process.env.APPDIRECT_ENCRYPTION_KEY;
const algorithm = "aes-256-cbc";
const IV_LENGTH = 16; // For AES, this is always 16

export function encrypt(text: string): string {
  if (!ENCRYPTION_KEY) {
    throw new Error("ENCRYPTION_KEY is not set");
  }

  const iv = crypto.randomBytes(IV_LENGTH);
  const cipher = crypto.createCipheriv(algorithm, ENCRYPTION_KEY, iv);

  let encryptedData = cipher.update(text, "utf-8", "hex");
  encryptedData += cipher.final("hex");

  return iv.toString("hex") + encryptedData; // Prepend IV to encrypted data
}

export function decrypt(text: string): string {
  if (!ENCRYPTION_KEY) {
    throw new Error("ENCRYPTION_KEY is not set");
  }

  const iv = Buffer.from(text.slice(0, 32), "hex"); // Extract the IV from the front
  const encryptedText = text.slice(32);

  const decipher = crypto.createDecipheriv(algorithm, ENCRYPTION_KEY, iv);

  let decryptedData = decipher.update(encryptedText, "hex", "utf-8");
  decryptedData += decipher.final("utf8");

  return decryptedData;
}
