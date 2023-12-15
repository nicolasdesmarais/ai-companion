import { GoogleDriveSearchRequest } from "@/src/adapter-in/api/GoogleDriveApi";
import { GoogleDriveDataSourceAdapter } from "@/src/adapter-out/knowledge/google-drive/GoogleDriveDataSourceAdapter";
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

  const body: GoogleDriveSearchRequest = await req.json();
  const { searchTerms, oauthTokenId } = body;

  try {
    const googleDriveLoader = new GoogleDriveDataSourceAdapter();
    const searchResponse = await googleDriveLoader.search(
      orgId,
      userId,
      oauthTokenId,
      searchTerms
    );

    return NextResponse.json(searchResponse);
  } catch (e) {
    console.log(e);
    if (e instanceof EntityNotFoundError) {
      return NextResponse.json({ folders: [], knowledgeIds: [] });
    }
    if (e instanceof BadRequestError) {
      return new NextResponse(e.message, { status: 400 });
    }

    return new NextResponse(e.message, { status: 500 });
  }
}
