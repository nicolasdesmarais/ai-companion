import { auth, currentUser } from "@clerk/nextjs";
import { NextResponse } from "next/server";
import { UserService } from '../../../../domain/services/UserService'


export async function GET(req: Request) {
  try {
    const user = await currentUser();

    if (!user || !user.id) {
      return new NextResponse("Unauthorized", { status: 401 });
    }

    const userService = new UserService();
    const userEntity = await userService.findUserByExternalId(user.id)

    return NextResponse.json(userEntity);
  } catch (error) {
    console.log("[ME_WORKSPACES_GET]", error);
    return new NextResponse("Internal Error", { status: 500 });
  }
};