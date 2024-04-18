import msftDataSourceAdapter from "@/src/adapter-out/knowledge/msft/MsftDataSourceAdapter";
import {
  BadRequestError,
  EntityNotFoundError,
} from "@/src/domain/errors/Errors";
import { auth } from "@clerk/nextjs";
import { NextResponse } from "next/server";

export const maxDuration = 300;

export async function POST(req: Request) {
  const { orgId, userId } = await auth();
  if (!orgId || !userId) {
    return new NextResponse("Unauthorized", { status: 401 });
  }

  const { searchTerm, oauthTokenId } = await req.json();

  try {
    const searchResponse = await msftDataSourceAdapter.search(
      userId,
      oauthTokenId,
      searchTerm
    );

    return NextResponse.json(searchResponse);
  } catch (e) {
    console.error("MSFT Search:", e.response?.data);
    if (e instanceof EntityNotFoundError) {
      return NextResponse.json({ folders: [], knowledgeIds: [] });
    }
    if (e instanceof BadRequestError) {
      return new NextResponse(e.message, { status: 400 });
    }

    return new NextResponse(e.message, { status: 500 });
  }
}
