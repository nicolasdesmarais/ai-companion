import {withErrorHandler} from "@/src/middleware/ErrorMiddleware";
import { clerkClient } from "@clerk/nextjs";
import { NextApiRequest, NextApiResponse } from "next";

async function postHandler(request: NextApiRequest, response: NextApiResponse) {
    const { userId, sortValue } = request.body;
    await clerkClient.users.updateUserMetadata(userId, {
        publicMetadata: {
            sortValue
        }
    })
    const user = await clerkClient.users.getUser(userId)
    console.log(user.publicMetadata);
    response.status(200).json({ success: true });
}


async function getHandler(request: NextApiRequest, response: NextApiResponse) {
    const { userId } = await request.body.json();
    const user = await clerkClient.users.getUser(userId)
    response.status(200).json(user.publicMetadata);
}

export const POST = withErrorHandler(postHandler);
export const GET = withErrorHandler(getHandler);