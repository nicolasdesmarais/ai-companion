import { decryptFromBuffer, encryptAsBuffer } from "@/src/lib/encryptionUtils";
import prismadb from "@/src/lib/prismadb";
import { OAuthTokenProvider } from "@prisma/client";

export class OrgClientCredentialsService {
  public async getOrgClientCredentialData(
    orgId: string,
    provider: OAuthTokenProvider
  ) {
    if (provider == OAuthTokenProvider.MSFT) {
      return {
        clientId: process.env.MSFT_CLIENT_ID,
        clientSecret: process.env.MSFT_SECRET,
        redirectUri: process.env.MSFT_REDIRECT,
      };
    }

    const orgClientCredentials = await prismadb.orgClientCredentials.findUnique(
      {
        where: { orgId_provider: { orgId, provider } },
      }
    );

    if (!orgClientCredentials?.data) {
      return null;
    }

    try {
      const decryptedData = decryptFromBuffer(orgClientCredentials.data);
      return JSON.parse(decryptedData);
    } catch (error) {
      console.log("Error while decrypting org client credentials", error);
    }
    return null;
  }

  public async upsertClientCredentials(
    orgId: string,
    provider: OAuthTokenProvider,
    data: any
  ) {
    const encryptedData = encryptAsBuffer(JSON.stringify(data));

    const credentials = await prismadb.orgClientCredentials.upsert({
      where: {
        orgId_provider: {
          orgId,
          provider,
        },
      },
      update: {
        data: encryptedData,
      },
      create: {
        orgId,
        provider,
        data: encryptedData,
      },
    });

    return {
      ...credentials,
      data,
    };
  }
}

const orgClientCredentialsService = new OrgClientCredentialsService();
export default orgClientCredentialsService;
