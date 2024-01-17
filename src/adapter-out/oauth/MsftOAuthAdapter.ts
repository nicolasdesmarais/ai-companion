import { OAuthAdapter, TokensFromRedirect } from "./OAuthAdapter";
import { OAuthTokenInfo } from "./OAuthTokenInfo";

const scope = "offline_access%20user.read%20files.read.all";
const msftUrl = "https://login.microsoftonline.com/common/oauth2/v2.0";
export class MsftOAuthAdapter implements OAuthAdapter {
  public getOAuthRedirectUrl(): string {
    return `${msftUrl}/authorize?client_id=${process.env.MSFT_CLIENT_ID}&redirect_uri=${process.env.MSFT_REDIRECT}&response_type=code&response_mode=query&scope=${scope}`;
  }

  public async getTokensFromRedirect(
    clientCredentialData: any,
    searchParams: URLSearchParams
  ): Promise<TokensFromRedirect> {
    const code = searchParams.get("code");
    const resp = await fetch(msftUrl + "/token", {
      method: "POST",
      headers: {
        "Content-Type": "application/x-www-form-urlencoded",
      },
      body: `client_id=${clientCredentialData.clientId}&scope=${scope}&code=${code}&redirect_uri=${clientCredentialData.redirectUri}&grant_type=authorization_code&client_secret=${clientCredentialData.clientSecret}`,
    });
    const {
      access_token,
      refresh_token,
      error,
      error_description,
      expires_in,
    } = await resp.json();

    const exp = new Date();
    exp.setSeconds(exp.getSeconds() + expires_in);

    if (error) {
      throw new Error(`MSFT Error: ${error} - ${error_description}`);
    }
    return { email: "CHANGE ME", tokens: { access_token, refresh_token, exp } };
  }

  public async getOAuthTokenInfo(
    clientCredentialData: any,
    token: any
  ): Promise<OAuthTokenInfo> {
    // TODO: refresh when needed
    return {
      isExistingTokenValid: true,
    };
  }
}

const msftOAuthAdapter = new MsftOAuthAdapter();
export default msftOAuthAdapter;
