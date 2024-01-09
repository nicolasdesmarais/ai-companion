import { OAuthAdapter, TokensFromRedirect } from "./OAuthAdapter";
import { OAuthTokenInfo } from "./OAuthTokenInfo";

export class MsftOAuthAdapter implements OAuthAdapter {
  public getOAuthRedirectUrl(): string {
    return `https://login.microsoftonline.com/common/oauth2/v2.0/authorize?client_id=${process.env.MSFT_CLIENT_ID}&redirect_uri=${process.env.MSFT_REDIRECT}&response_type=code&response_mode=query&scope=offline_access%20user.read`;
  }

  public async getTokensFromRedirect(
    clientCredentialData: any,
    searchParams: URLSearchParams
  ): Promise<TokensFromRedirect> {
    return { email: "", tokens: null };
  }

  public async getOAuthTokenInfo(
    clientCredentialData: any,
    token: any
  ): Promise<OAuthTokenInfo> {
    return {
      isExistingTokenValid: false,
    };
  }
}

const msftOAuthAdapter = new MsftOAuthAdapter();
export default msftOAuthAdapter;
