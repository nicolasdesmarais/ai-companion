import { Credentials } from "google-auth-library";
import { google } from "googleapis";
import { googleDriveOauth2Client } from "./GoogleDriveClient";
import { OAuthAdapter, TokensFromRedirect } from "./OAuthAdapter";
import { OAuthTokenInfo } from "./OAuthTokenInfo";

const SCOPE = [
  "https://www.googleapis.com/auth/drive.readonly",
  "https://www.googleapis.com/auth/userinfo.email",
  "https://www.googleapis.com/auth/userinfo.profile",
];

export class GoogleDriveOAuthAdapter implements OAuthAdapter {
  public getOAuthRedirectUrl(clientCredentialData: any): string {
    const oauth2Client = googleDriveOauth2Client(clientCredentialData);

    return oauth2Client.generateAuthUrl({
      prompt: "consent",
      access_type: "offline",
      scope: SCOPE,
    });
  }

  public async getTokensFromRedirect(
    clientCredentialData: any,
    searchParams: URLSearchParams
  ): Promise<TokensFromRedirect> {
    const oauth2Client = googleDriveOauth2Client(clientCredentialData);

    const code = searchParams.get("code");
    const { tokens } = await oauth2Client.getToken(code as string);
    const email = await this.getEmailAddressFromToken(
      clientCredentialData,
      tokens
    );
    if (!email) {
      throw new Error("Unable to get email address from token");
    }

    return {
      email,
      tokens,
    };
  }

  private async getEmailAddressFromToken(
    clientCredentialData: any,
    tokens: Credentials
  ) {
    const oauth2Client = googleDriveOauth2Client(clientCredentialData);
    oauth2Client.setCredentials(tokens);

    const people = google.people({ version: "v1", auth: oauth2Client });

    const user = await people.people.get({
      resourceName: "people/me",
      personFields: "emailAddresses",
    });

    for (const emailAddress of user.data.emailAddresses || []) {
      if (emailAddress.metadata?.primary) {
        return emailAddress.value;
      }
    }
  }

  public async getOAuthTokenInfo(
    clientCredentialData: any,
    token: any
  ): Promise<OAuthTokenInfo> {
    const oauthTokenData = token as {
      access_token: string;
      refresh_token: string;
    };

    const oauth2Client = googleDriveOauth2Client(clientCredentialData);
    try {
      await oauth2Client.getTokenInfo(oauthTokenData.access_token);
      return {
        isExistingTokenValid: true,
      };
    } catch (error) {
      oauth2Client.setCredentials({
        access_token: oauthTokenData.access_token,
        refresh_token: oauthTokenData.refresh_token,
      });

      try {
        const refreshedToken = await oauth2Client.refreshAccessToken();
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
