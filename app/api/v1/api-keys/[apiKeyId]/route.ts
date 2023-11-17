import apiKeyService from "@/src/domain/services/ApiKeyService";
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
