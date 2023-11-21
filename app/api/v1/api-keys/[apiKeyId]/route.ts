import {
  EntityNotFoundError,
  ForbiddenError,
} from "@/src/domain/errors/Errors";
import apiKeyService from "@/src/domain/services/ApiKeyService";
import { UpdateApiKeyRequest } from "@/src/ports/api/ApiKeysApi";
import { auth } from "@clerk/nextjs";
import { NextResponse } from "next/server";

export async function DELETE(
  request: Request,
  { params }: { params: { apiKeyId: string } }
) {
  try {
    const { userId, orgId } = auth();

    if (!userId || !orgId) {
      return new NextResponse("Unauthorized", { status: 401 });
    }

    await apiKeyService.deleteApiKey(orgId, userId, params.apiKeyId);

    return new NextResponse(null, { status: 204 });
  } catch (error) {
    console.log("[API KEY DELETE]", error);
    return new NextResponse("Internal Error", { status: 500 });
  }
}

export async function PUT(
  request: Request,
  { params }: { params: { apiKeyId: string } }
) {
  try {
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
  } catch (error) {
    if (error instanceof EntityNotFoundError) {
      return new NextResponse("Not Found", { status: 404 });
    }
    if (error instanceof ForbiddenError) {
      return new NextResponse("Forbidden", { status: 403 });
    }

    console.log("[API KEY UPDATE]", error);
    return new NextResponse("Internal Error", { status: 500 });
  }
}
