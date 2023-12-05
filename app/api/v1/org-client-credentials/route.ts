import { CreateOrgClientCredentialsRequest as UpsertOrgClientCredentialsRequest } from "@/src/domain/ports/api/OrgClientCredentialsApi";
import orgClientCredentialsService from "@/src/domain/services/OrgClientCredentialsService";
import { AuthorizationScope } from "@/src/domain/types/AuthorizationContext";
import { withAuthorization } from "@/src/middleware/AuthorizationMiddleware";
import { withErrorHandler } from "@/src/middleware/ErrorMiddleware";
import { NextResponse } from "next/server";

async function postHandler(
  req: Request,
  context: { orgId: string; userId: string }
) {
  const { orgId, userId } = context;

  const body: UpsertOrgClientCredentialsRequest = await req.json();

  const clientCredentials =
    await orgClientCredentialsService.upsertClientCredentials(
      orgId,
      body.provider,
      body.data
    );
  return NextResponse.json(clientCredentials, { status: 201 });
}

export const POST = withErrorHandler(
  withAuthorization(
    AuthorizationScope.ORG_CLIENT_CREDENTIALS_WRITE,
    postHandler
  )
);
