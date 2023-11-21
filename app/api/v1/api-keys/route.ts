import apiKeyService from "@/src/domain/services/ApiKeyService";
import { withErrorHandler } from "@/src/middleware/ErrorMiddleware";
import { CreateApiKeyRequest } from "@/src/ports/api/ApiKeysApi";
import { auth } from "@clerk/nextjs";
import { NextResponse } from "next/server";

async function postHandler(req: Request) {
  const authn = await auth();
  const { orgId, userId } = authn;

  if (!orgId || !userId) {
    return new NextResponse("Unauthorized", { status: 401 });
  }

  const body: CreateApiKeyRequest = await req.json();

  const apiKey = await apiKeyService.createApiKey(orgId, userId, body);
  return NextResponse.json(apiKey, { status: 201 });
}

export const POST = withErrorHandler(postHandler);
