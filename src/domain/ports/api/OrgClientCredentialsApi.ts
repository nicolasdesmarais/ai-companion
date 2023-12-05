import { OAuthTokenProvider } from "@prisma/client";

export interface CreateOrgClientCredentialsRequest {
  provider: OAuthTokenProvider;
  data: any;
}
