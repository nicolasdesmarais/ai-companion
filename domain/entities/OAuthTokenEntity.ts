import { OAuthTokenProvider } from "@prisma/client";

export interface UserOAuthTokenEntity {
  id?: string;
  userId: string;
  provider: OAuthTokenProvider;
  email: string;
  data: any;
  createdAt?: Date;
  updatedAt?: Date;
}
