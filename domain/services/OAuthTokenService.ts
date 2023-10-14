import prismadb from "@/lib/prismadb";
import { UserOAuthTokenEntity as OAuthTokenEntity } from "../entities/OAuthTokenEntity";

export class OAuthTokenService {
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
