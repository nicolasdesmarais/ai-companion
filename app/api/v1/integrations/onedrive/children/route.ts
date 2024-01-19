import msftDataSourceAdapter from "@/src/adapter-out/knowledge/msft/MsftDataSourceAdapter";
import {
  BadRequestError,
  EntityNotFoundError,
} from "@/src/domain/errors/Errors";
import { withAuthorization } from "@/src/middleware/AuthorizationMiddleware";
import { withErrorHandler } from "@/src/middleware/ErrorMiddleware";
import { AuthorizationContext } from "@/src/security/models/AuthorizationContext";
import { SecuredAction } from "@/src/security/models/SecuredAction";
import { SecuredResourceAccessLevel } from "@/src/security/models/SecuredResourceAccessLevel";
import { SecuredResourceType } from "@/src/security/models/SecuredResourceType";
import { auth } from "@clerk/nextjs";
import { NextResponse } from "next/server";

export async function postHandler(
  req: Request,
  context: {
    authorizationContext: AuthorizationContext;
  }
) {
  const { id, oauthTokenId } = await req.json();
  const { userId } = context.authorizationContext;

  try {
    const response = await msftDataSourceAdapter.children(
      userId,
      oauthTokenId,
      id
    );

    return NextResponse.json(response);
  } catch (e) {
    console.error("MSFT children:", e.response?.data);
    if (e instanceof EntityNotFoundError) {
      return NextResponse.json({ folders: [], knowledgeIds: [] });
    }
    if (e instanceof BadRequestError) {
      return new NextResponse(e.message, { status: 400 });
    }

    return new NextResponse(e.message, { status: 500 });
  }
}
export const POST = withErrorHandler(
  withAuthorization(
    SecuredResourceType.DATA_SOURCES,
    SecuredAction.READ,
    [SecuredResourceAccessLevel.SELF],
    postHandler
  )
);
