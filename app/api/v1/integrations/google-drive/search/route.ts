import { GoogleDriveDataSourceAdapter } from "@/src/adapters/knowledge/google-drive/GoogleDriveDataSourceAdapter";
import {
  BadRequestError,
  EntityNotFoundError,
} from "@/src/domain/errors/Errors";
import { GoogleDriveSearchRequest } from "@/src/ports/api/GoogleDriveApi";
import { currentUser } from "@clerk/nextjs";
import { NextResponse } from "next/server";

export const maxDuration = 300;

export async function POST(req: Request) {
  const user = await currentUser();
  if (!user) {
    return new NextResponse("Unauthorized", { status: 401 });
  }

  const userId = user.id;
  const body: GoogleDriveSearchRequest = await req.json();
  const { searchTerms, oauthTokenId } = body;

  try {
    const googleDriveLoader = new GoogleDriveDataSourceAdapter();
    const searchResponse = await googleDriveLoader.search(
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
