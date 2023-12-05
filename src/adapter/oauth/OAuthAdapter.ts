import { OAuthTokenInfo } from "./OAuthTokenInfo";

export interface OAuthAdapter {
  getOAuthRedirectUrl(orgId: string): string;

  getTokensFromRedirect(
    clientCredentialData: any,
    searchParams: URLSearchParams
  ): Promise<TokensFromRedirect>;

  getOAuthTokenInfo(
    clientCredentialData: any,
    token: any
  ): Promise<OAuthTokenInfo>;
}

export interface TokensFromRedirect {
  email: string;
  tokens: any;
}
