import {withErrorHandler} from "@/src/middleware/ErrorMiddleware";
import { clerkClient } from "@clerk/nextjs";
import { NextApiRequest, NextApiResponse } from "next";
import {withAuthorization} from "@/src/middleware/AuthorizationMiddleware";
import {SecuredResourceType} from "@/src/security/models/SecuredResourceType";
import {SecuredAction} from "@/src/security/models/SecuredAction";
import {SecuredResourceAccessLevel} from "@/src/security/models/SecuredResourceAccessLevel";
import {NextResponse} from "next/server";

async function postHandler(request: Request)  {
    console.log("Body: ", await request.json())
    const { userId, sortValue } = await request.json();
    await clerkClient.users.updateUserMetadata(userId, {
        publicMetadata: {
            sort: sortValue
        }
    })

    const user = await clerkClient.users.getUser(userId)
    console.log("Public MetaData ",user.publicMetadata);
    return NextResponse.json({ success: true, user : user.publicMetadata });
}


async function getHandler(request: NextApiRequest, response: NextApiResponse) {
    const { userId } = await request.body.json();
    const user = await clerkClient.users.getUser(userId)
    response.status(200).json(user.publicMetadata);
}

export const POST = withErrorHandler(
    withAuthorization(
        SecuredResourceType.ORG_USAGE,
        SecuredAction.WRITE,
        Object.values(SecuredResourceAccessLevel),
        postHandler));

export const GET = withErrorHandler(
    withAuthorization(
    SecuredResourceType.ORG_USAGE,
    SecuredAction.WRITE,
    Object.values(SecuredResourceAccessLevel),
    getHandler)
);