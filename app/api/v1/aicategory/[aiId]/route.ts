import prismadb from "@/src/lib/prismadb";
import {NextResponse} from "next/server";
import {withErrorHandler} from "@/src/middleware/ErrorMiddleware";
import {AuthorizationContext} from "@/src/security/models/AuthorizationContext";
import {AICategoryTypeInterface} from "@/app/api/v1/aicategory/route";

async function getHandler(request: Request, context: { params: { aiId: string }; authorizationContext: AuthorizationContext; }){
    const { params, authorizationContext } = context;
    const aiId = params.aiId;
    const data : Array<AICategoryTypeInterface> = await prismadb.aICategoryType.findMany({where: {aiId}});
    return NextResponse.json(data);
}

export const GET = withErrorHandler(getHandler);