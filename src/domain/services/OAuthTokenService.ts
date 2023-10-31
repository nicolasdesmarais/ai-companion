import googleDriveOAuthAdapter from "@/src/adapters/oauth/GoogleDriveOAuthAdapter";
import { OAuthAdapter } from "@/src/adapters/oauth/OAuthAdapter";
import { decryptFromBuffer, encryptAsBuffer } from "@/src/lib/encryptionUtils";
import prismadb from "@/src/lib/prismadb";
import { OAuthTokenProvider } from "@prisma/client";
import { UserOAuthTokenEntity } from "../entities/OAuthTokenEntity";

export class OAuthTokenService {
  private getOAuthAdapter(provider: OAuthTokenProvider): OAuthAdapter {
    switch (provider) {
      case OAuthTokenProvider.GOOGLE:
        return googleDriveOAuthAdapter;
      default:
        throw new Error(`No OAuthAdapter found for ${provider}`);
    }
  }

  public async getOAuthTokens(
    provider: OAuthTokenProvider,
    userId: string
  ): Promise<UserOAuthTokenEntity[]> {
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
      const tokenData = JSON.parse(decryptFromBuffer(token.data));
      const isValid = await oauthAdapter.validateToken(tokenData);
      if (isValid) {
        validTokens.push(token);
      }
    }

    return validTokens;
  }

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
}

const oauthTokenService = new OAuthTokenService();
export default oauthTokenService;
