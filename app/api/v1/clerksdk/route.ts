import clerkService from "@/src/domain/services/ClerkService";
import {withErrorHandler} from "@/src/middleware/ErrorMiddleware";
import { NextResponse } from "next/server";
import {UserMetadataParams} from "@/src/domain/services/ClerkService";

async function postHandler(req: Request, context: {params: {userId : string, metaDataParam: UserMetadataParams}}) {
    const userId = context.params.userId;
    const metaDataParam = context.params.metaDataParam;
    const response = await clerkService.updateUserMetadata(userId, metaDataParam);
    return NextResponse.json(response);
}

async function getHandler(req: Request, context: {params: {userId : string}}) {
    const userId = context.params.userId;
    const response = await clerkService.getUserMetadata(userId);
    return NextResponse.json(response);
}

export const POST = withErrorHandler(postHandler);
export const GET = withErrorHandler(getHandler);