import { OAuthTokenInfo } from "./OAuthTokenInfo";

export interface OAuthAdapter {
  getOAuthTokenInfo(token: any): Promise<OAuthTokenInfo>;
}
