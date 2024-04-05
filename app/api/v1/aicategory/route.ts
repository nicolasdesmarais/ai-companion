import {withErrorHandler} from "@/src/middleware/ErrorMiddleware";
import prismadb from "@/src/lib/prismadb";
import {CategoryType} from "@prisma/client";
import {NextResponse} from "next/server";
import {randomUUID} from "node:crypto";
import {withAuthorization} from "@/src/middleware/AuthorizationMiddleware";
import {SecuredResourceType} from "@/src/security/models/SecuredResourceType";
import {SecuredAction} from "@/src/security/models/SecuredAction";
import {SecuredResourceAccessLevel} from "@/src/security/models/SecuredResourceAccessLevel";
import {getUserAuthorizationContext} from "@/src/security/utils/securityUtils";

async function putHandler(req: Request) {
    const [aiId, categoryType] = await req.json();
    await prismadb.aICategoryType.update({where: {aiId}, data: {categoryType}});
    const data = await prismadb.aICategoryType.findMany({where: {aiId}});
    return NextResponse.json(data);
}

async function deleteHandler(req: Request) {
    const aiId = await req.json();
    await prismadb.aICategoryType.delete({where: {aiId}});
    return NextResponse.json({aiId});

}
async function postHandler(  req: Request ) {
    const request = await req.json();
    const aiId = request.aiId;
    const categoryType = request.categoryType;
    await prismadb.aICategoryType.create({data : {aiId, categoryType}});
    const data =  await prismadb.aICategoryType.findMany({where: {aiId}});
    return NextResponse.json(data);
}

// create curl for above post method
// curl -X POST http://localhost:3000/api/v1/aicategory/ -H "Content-Type: application/json" -d '["1", "TEXT"]'
export const POST = withErrorHandler(postHandler);
// export const POST = withErrorHandler(
//     withAuthorization(
//         SecuredResourceType.AI,
//         SecuredAction.WRITE,
//         Object.values(SecuredResourceAccessLevel),
//         postHandler
//     )
// );

export const DELETE = withErrorHandler(deleteHandler);
export const PUT = withErrorHandler(putHandler);