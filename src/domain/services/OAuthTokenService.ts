import googleDriveOAuthAdapter from "@/src/adapter/oauth/GoogleDriveOAuthAdapter";
import { OAuthAdapter } from "@/src/adapter/oauth/OAuthAdapter";
import { decryptFromBuffer, encryptAsBuffer } from "@/src/lib/encryptionUtils";
import prismadb from "@/src/lib/prismadb";
import { OAuthTokenProvider } from "@prisma/client";
import { UserOAuthTokenEntity } from "../entities/OAuthTokenEntity";
import orgClientCredentialsService from "./OrgClientCredentialsService";

export class OAuthTokenService {
  private getOAuthAdapter(provider: OAuthTokenProvider): OAuthAdapter {
    switch (provider) {
      case OAuthTokenProvider.GOOGLE:
        return googleDriveOAuthAdapter;
      default:
        throw new Error(`No OAuthAdapter found for ${provider}`);
    }
  }

  private async getOrgClientCredentialData(
    orgId: string,
    provider: OAuthTokenProvider
  ) {
    const orgClientCredentialData =
      await orgClientCredentialsService.getOrgClientCredentialData(
        orgId,
        provider
      );

    if (!orgClientCredentialData) {
      throw new Error(
        `Missing client credentials for ${provider}, for org ${orgId}`
      );
    }

    return orgClientCredentialData;
  }
  /**
   * Return a list of OAuth tokens for the given user and provider.
   * @param orgId
   * @param userId
   * @param provider
   * @returns
   */
  public async getOAuthTokens(
    orgId: string,
    userId: string,
    provider: OAuthTokenProvider
  ): Promise<UserOAuthTokenEntity[]> {
    const orgClientCredentialData = this.getOrgClientCredentialData(
      orgId,
      provider
    );

    const oauthAdapter = await this.getOAuthAdapter(provider);

    const tokens = await prismadb.oAuthToken.findMany({
      where: {
        provider,
        userId,
      },
    });

    const validTokens: UserOAuthTokenEntity[] = [];
    for (const token of tokens) {
      if (!token.data) {
        continue;
      }
      try {
        const tokenData = JSON.parse(decryptFromBuffer(token.data));
        const tokenInfo = await oauthAdapter.getOAuthTokenInfo(
          orgClientCredentialData,
          tokenData
        );
        if (tokenInfo.isExistingTokenValid) {
          validTokens.push(token);
        } else if (tokenInfo.refreshedToken) {
          await this.upsertToken({
            ...token,
            data: tokenInfo.refreshedToken,
          });
          validTokens.push(token);
        }
      } catch (error) {
        console.error(error);
      }
    }

    return validTokens;
  }

  /**
   * Upsert the OAuth token for the given user, email and provider
   * @param token
   */
  public async upsertToken(token: UserOAuthTokenEntity) {
    const encryptedData = encryptAsBuffer(JSON.stringify(token.data));

    await prismadb.oAuthToken.upsert({
      where: {
        provider_userId_email: {
          provider: token.provider,
          userId: token.userId,
          email: token.email,
        },
      },
      update: {
        data: encryptedData,
      },
      create: {
        userId: token.userId,
        provider: token.provider,
        email: token.email,
        data: encryptedData,
      },
    });
  }

  public async getOAuthRedirectUrl(
    orgId: string,
    provider: OAuthTokenProvider
  ) {
    const oauthAdapter = await this.getOAuthAdapter(provider);
    const orgClientCredentialData = await this.getOrgClientCredentialData(
      orgId,
      provider
    );

    return oauthAdapter.getOAuthRedirectUrl(orgClientCredentialData);
  }
}

const oauthTokenService = new OAuthTokenService();
export default oauthTokenService;
