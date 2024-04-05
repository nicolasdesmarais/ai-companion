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

export interface AICategoryTypeInterface {
    aiId: string;
    categoryType: CategoryType;
}

async function postHandler(  req: Request ) {
    const {aiId, categoryType} : AICategoryTypeInterface = await req.json();
    await prismadb.aICategoryType.create({data : {aiId, categoryType}});
    const data : Array<AICategoryTypeInterface> =  await prismadb.aICategoryType.findMany({where: {aiId}});
    return NextResponse.json(data);
}

async function deleteHandler( req: Request ) {
    const reqBody = await req.json();
    const aiId = reqBody[0].aiId;
    reqBody.map(async (item : AICategoryTypeInterface) => {
        const {aiId, categoryType} : AICategoryTypeInterface = item;
        await prismadb.aICategoryType.deleteMany({where : {aiId, categoryType}});
    });
    const data: Array<AICategoryTypeInterface> =await prismadb.aICategoryType.findMany({where: {aiId}});
    return NextResponse.json(data);
}

// export const POST = withErrorHandler(
//     withAuthorization(
//         SecuredResourceType.AI,
//         SecuredAction.WRITE,
//         Object.values(SecuredResourceAccessLevel),
//         postHandler
//     )
// );

export const POST = withErrorHandler(postHandler);
export const DELETE = withErrorHandler(deleteHandler);