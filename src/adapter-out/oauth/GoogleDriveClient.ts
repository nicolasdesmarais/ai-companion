import { google } from "googleapis";

const GOOGLE_CALLBACK_URL = process.env.GOOGLE_CALLBACK_URL;

interface GoogleDriveClientCredentialData {
  clientId: string;
  clientSecret: string;
}

export const googleDriveOauth2Client = (clientCredentialData: any) => {
  const { clientId, clientSecret } =
    clientCredentialData as GoogleDriveClientCredentialData;

  return new google.auth.OAuth2(clientId, clientSecret, GOOGLE_CALLBACK_URL);
};

export const googleDriveClient = (oauth2Client: any) => {
  return google.drive({ version: "v3", auth: oauth2Client });
};
