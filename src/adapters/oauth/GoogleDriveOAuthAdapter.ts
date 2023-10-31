import { google } from "googleapis";
import { OAuthAdapter } from "./OAuthAdapter";

const OAUTH2_CLIENT = new google.auth.OAuth2(
  process.env.GOOGLE_CLIENT_ID,
  process.env.GOOGLE_CLIENT_SECRET,
  process.env.GOOGLE_CALLBACK_URL
);

export class GoogleDriveOAuthAdapter implements OAuthAdapter {
  public async validateToken(token: any): Promise<boolean> {
    const oauthTokenData = token as {
      access_token: string;
      refresh_token: string;
    };

    try {
      const tokenInfo = await OAUTH2_CLIENT.getTokenInfo(
        oauthTokenData.access_token
      );
      console.log(tokenInfo);
      return true;
    } catch (error) {
      console.log(error);
      return false;
    }
  }
}

const googleDriveOAuthAdapter = new GoogleDriveOAuthAdapter();
export default googleDriveOAuthAdapter;
