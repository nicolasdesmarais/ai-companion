import prismadb from "@/src/lib/prismadb";
import { withAuthorization } from "@/src/middleware/AuthorizationMiddleware";
import { withErrorHandler } from "@/src/middleware/ErrorMiddleware";
import { SecuredAction } from "@/src/security/models/SecuredAction";
import { SecuredResourceAccessLevel } from "@/src/security/models/SecuredResourceAccessLevel";
import { SecuredResourceType } from "@/src/security/models/SecuredResourceType";
import { Category } from "@prisma/client";
import { NextResponse } from "next/server";

async function getHandler(req: Request): Promise<NextResponse<Category[]>> {
  const categories = await prismadb.category.findMany();
  return NextResponse.json(categories);
}

export const GET = withErrorHandler(
  withAuthorization(
    SecuredResourceType.GROUPS,
    SecuredAction.READ,
    Object.values(SecuredResourceAccessLevel),
    getHandler
  )
);
