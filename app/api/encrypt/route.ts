import { decrypt, encrypt } from "@/src/lib/encryptionUtils";

export async function GET(req: Request) {
  const encryptedData = encrypt("test");
  console.log("encryptedData", encryptedData);

  const decryptedData = decrypt(encryptedData);
  console.log("decryptedData", decryptedData);
}
