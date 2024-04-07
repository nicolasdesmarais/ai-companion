import {withErrorHandler} from "@/src/middleware/ErrorMiddleware";
import prismadb from "@/src/lib/prismadb";
import {CategoryType} from "@prisma/client";
import {NextRequest, NextResponse} from "next/server";
import {withAuthorization} from "@/src/middleware/AuthorizationMiddleware";
import {SecuredResourceType} from "@/src/security/models/SecuredResourceType";
import {SecuredAction} from "@/src/security/models/SecuredAction";
import {SecuredResourceAccessLevel} from "@/src/security/models/SecuredResourceAccessLevel";
import {getUserAuthorizationContext} from "@/src/security/utils/securityUtils";
import {clerkClient} from "@clerk/nextjs";

export interface AICategoryTypeInterface {
    aiId: string;
    categoryType: CategoryType;
}

async function postHandler(  request: Request ) {
    const authorizationContext = getUserAuthorizationContext();
    if (!authorizationContext) { return; }
    let aiIdGet : string = "7cce1fa-7a32-4da5-9136-eb9c5c0c9889";
    let areRecordsDeleted : boolean = false;
    const paramArray : Array<AICategoryTypeInterface> = await request.json();
    paramArray.map(async (param) => {
        const aiId : any = param.aiId;
        aiIdGet = aiId;
        const categoryType : CategoryType = param.categoryType;
        if (!areRecordsDeleted) {
            await prismadb.aICategoryType.deleteMany({where: {aiId: aiId}});
            areRecordsDeleted = true;
        }
        await prismadb.aICategoryType.create({data : { aiId, categoryType }});
    });
    const data : Array<AICategoryTypeInterface> =  await prismadb.aICategoryType.findMany({where: {aiId: aiIdGet}});
    return NextResponse.json(data);
}

async function getHandler(request: NextRequest) {
    const authorizationContext = getUserAuthorizationContext();
    if (!authorizationContext) {
        return;
    }
    const { searchParams } = new URL(request.url);
    const scopeParamaiId : string | null = searchParams.get("aiId");
    const data : Array<AICategoryTypeInterface> = await clerkClient.aICategoryType.findMany({where: {aiId: scopeParamaiId}});
    return NextResponse.json(data);
}

export const POST = withErrorHandler(
    withAuthorization(
        SecuredResourceType.AI,
        SecuredAction.WRITE,
        Object.values(SecuredResourceAccessLevel),
        postHandler
    )
);

export const GET = withErrorHandler(
    withAuthorization(
        SecuredResourceType.AI,
        SecuredAction.READ,
        Object.values(SecuredResourceAccessLevel),
        getHandler
    )
);