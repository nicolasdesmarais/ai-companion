import prismadb from "@/src/lib/prismadb";
import { OAuthTokenProvider } from "@prisma/client";
import { UserOAuthTokenEntity } from "../entities/OAuthTokenEntity";

export class OAuthTokenService {
  public async getOAuthTokens(
    provider: OAuthTokenProvider,
    userId: string
  ): Promise<UserOAuthTokenEntity[]> {
    return await prismadb.oAuthToken.findMany({
      select: {
        id: true,
        userId: true,
        email: true,
        provider: true,
      },
      where: {
        provider,
        userId,
      },
    });
  }

  public async upsertToken(token: UserOAuthTokenEntity) {
    await prismadb.oAuthToken.upsert({
      where: {
        provider_userId_email: {
          provider: token.provider,
          userId: token.userId,
          email: token.email,
        },
      },
      update: {
        data: token.data,
      },
      create: {
        userId: token.userId,
        provider: token.provider,
        email: token.email,
        data: token.data,
      },
    });
  }
}
