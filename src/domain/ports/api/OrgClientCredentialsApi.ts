import { OAuthTokenProvider } from "@prisma/client";

export interface UpsertClientCredentialsRequest {
  provider: OAuthTokenProvider;
  data: any;
}
