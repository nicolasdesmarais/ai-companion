import { google } from "googleapis";
import { OAuthAdapter } from "./OAuthAdapter";
import { OAuthTokenInfo } from "./OAuthTokenInfo";

const OAUTH2_CLIENT = new google.auth.OAuth2(
  process.env.GOOGLE_CLIENT_ID,
  process.env.GOOGLE_CLIENT_SECRET,
  process.env.GOOGLE_CALLBACK_URL
);

export class GoogleDriveOAuthAdapter implements OAuthAdapter {
  public async getOAuthTokenInfo(token: any): Promise<OAuthTokenInfo> {
    const oauthTokenData = token as {
      access_token: string;
      refresh_token: string;
    };

    try {
      await OAUTH2_CLIENT.getTokenInfo(oauthTokenData.access_token);
      return {
        isExistingTokenValid: true,
      };
    } catch (error) {
      const client = new google.auth.OAuth2(
        process.env.GOOGLE_CLIENT_ID,
        process.env.GOOGLE_CLIENT_SECRET,
        process.env.GOOGLE_CALLBACK_URL
      );
      client.setCredentials({
        access_token: oauthTokenData.access_token,
        refresh_token: oauthTokenData.refresh_token,
      });

      try {
        const refreshedToken = await client.refreshAccessToken();
        return {
          isExistingTokenValid: false,
          refreshedToken: {
            access_token: refreshedToken.credentials.access_token,
            refresh_token: oauthTokenData.refresh_token,
          },
        };
      } catch (error) {
        console.error(error);
      }

      return {
        isExistingTokenValid: false,
      };
    }
  }
}

const googleDriveOAuthAdapter = new GoogleDriveOAuthAdapter();
export default googleDriveOAuthAdapter;
