import { UpdateApiKeyRequest } from "@/src/domain/models/ApiKeysApi";
import apiKeyService from "@/src/domain/services/ApiKeyService";
import { withErrorHandler } from "@/src/middleware/ErrorMiddleware";
import { auth } from "@clerk/nextjs";
import { NextResponse } from "next/server";

async function putHandler(
  request: Request,
  { params }: { params: { apiKeyId: string } }
) {
  const { userId, orgId } = auth();

  if (!userId || !orgId) {
    return new NextResponse("Unauthorized", { status: 401 });
  }

  const body: UpdateApiKeyRequest = await request.json();

  const updatedKey = await apiKeyService.updateApiKey(
    orgId,
    userId,
    params.apiKeyId,
    body
  );

  return NextResponse.json(updatedKey);
}

async function deleteHandler(
  request: Request,
  { params }: { params: { apiKeyId: string } }
) {
  const { userId, orgId } = auth();

  if (!userId || !orgId) {
    return new NextResponse("Unauthorized", { status: 401 });
  }

  await apiKeyService.deleteApiKey(orgId, userId, params.apiKeyId);

  return new NextResponse(null, { status: 204 });
}

export const PUT = withErrorHandler(putHandler);
export const DELETE = withErrorHandler(deleteHandler);
