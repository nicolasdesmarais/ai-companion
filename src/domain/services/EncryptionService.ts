import crypto from "crypto";

const ENCRYPTION_KEY = process.env.APPDIRECT_ENCRYPTION_KEY;
const IV_LENGTH = 16; // For AES, this is always 16

export class EncryptionService {
  public encrypt(text: string): string {
    if (!ENCRYPTION_KEY) {
      throw new Error("ENCRYPTION_KEY is not set");
    }

    const iv = crypto.randomBytes(IV_LENGTH);
    const cipher = crypto.createCipheriv(
      "aes-256-gcm",
      Buffer.from(ENCRYPTION_KEY, "hex"),
      iv
    );
    let encrypted = cipher.update(text);
    encrypted = Buffer.concat([encrypted, cipher.final()]);
    return iv.toString("hex") + ":" + encrypted.toString("hex");
  }

  public decrypt(text: string): string {
    if (!ENCRYPTION_KEY) {
      throw new Error("ENCRYPTION_KEY is not set");
    }

    const textParts = text.split(":");
    const iv = Buffer.from(textParts.shift()!, "hex");
    const encryptedText = Buffer.from(textParts.join(":"), "hex");
    const decipher = crypto.createDecipheriv(
      "aes-256-gcm",
      Buffer.from(ENCRYPTION_KEY, "hex"),
      iv
    );
    let decrypted = decipher.update(encryptedText);
    decrypted = Buffer.concat([decrypted, decipher.final()]);
    return decrypted.toString();
  }
}
