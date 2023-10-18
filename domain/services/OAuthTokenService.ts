import prismadb from "@/lib/prismadb";
import { OAuthTokenProvider } from "@prisma/client";
import { UserOAuthTokenEntity as OAuthTokenEntity } from "../entities/OAuthTokenEntity";

export class OAuthTokenService {
  public async hasOAuthToken(provider: OAuthTokenProvider, userId: string) {
    const token = await prismadb.oAuthToken.findUnique({
      where: {
        provider_userId: {
          provider,
          userId,
        },
      },
    });

    return !!token;
  }

  public async upsertToken(token: OAuthTokenEntity) {
    await prismadb.oAuthToken.upsert({
      where: {
        provider_userId: {
          provider: token.provider,
          userId: token.userId,
        },
      },
      update: {
        data: token.data,
      },
      create: {
        userId: token.userId,
        provider: token.provider,
        data: token.data,
      },
    });
  }
}
