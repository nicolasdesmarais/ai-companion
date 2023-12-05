import { OAuthTokenInfo } from "./OAuthTokenInfo";

export interface OAuthAdapter {
  getOAuthTokenInfo(
    clientCredentialData: any,
    token: any
  ): Promise<OAuthTokenInfo>;
}
