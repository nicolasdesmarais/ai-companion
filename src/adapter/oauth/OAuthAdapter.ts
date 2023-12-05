import { OAuthTokenInfo } from "./OAuthTokenInfo";

export interface OAuthAdapter {
  getOAuthRedirectUrl(orgId: string): string;

  getOAuthTokenInfo(
    clientCredentialData: any,
    token: any
  ): Promise<OAuthTokenInfo>;
}
