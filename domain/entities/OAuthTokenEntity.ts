import { OAuthTokenProvider } from "@prisma/client";

export interface UserOAuthTokenEntity {
  id?: string;
  userId: string;
  provider: OAuthTokenProvider;
  data: any;
  createdAt?: Date;
  updatedAt?: Date;
}
