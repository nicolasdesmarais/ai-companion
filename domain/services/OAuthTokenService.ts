import prismadb from "@/lib/prismadb";
import { OAuthTokenProvider } from "@prisma/client";
import { UserOAuthTokenEntity } from "../entities/OAuthTokenEntity";

export class OAuthTokenService {
  public async getOAuthTokenEmails(
    provider: OAuthTokenProvider,
    userId: string
  ) {
    return await prismadb.oAuthToken.findMany({
      select: {
        email: true,
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
