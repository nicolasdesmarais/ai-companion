import apiKeyService from "@/src/domain/services/ApiKeyService";
import { CreateApiKeyRequest } from "@/src/ports/api/ApiKeysApi";
import { auth } from "@clerk/nextjs";
import { NextResponse } from "next/server";

export async function POST(req: Request) {
  const authn = await auth();
  const { orgId, userId } = authn;

  if (!orgId || !userId) {
    return new NextResponse("Unauthorized", { status: 401 });
  }

  const body: CreateApiKeyRequest = await req.json();

  try {
    const apiKey = await apiKeyService.createApiKey(orgId, userId, body);
    return NextResponse.json(apiKey, { status: 201 });
  } catch (e) {
    return new NextResponse("Internal Server Error", { status: 500 });
  }
}
