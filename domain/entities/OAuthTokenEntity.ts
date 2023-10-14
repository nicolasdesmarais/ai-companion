import { OAuthTokenProvider } from "@prisma/client";

export interface UserOAuthTokenEntity {
  id?: string;
  userId: string;
  provider: OAuthTokenProvider;
  data: string;
  createdAt?: Date;
  updatedAt?: Date;
}
