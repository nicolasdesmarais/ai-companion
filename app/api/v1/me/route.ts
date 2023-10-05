import { currentUser } from "@clerk/nextjs";
import { NextResponse } from "next/server";

export async function GET(req: Request) {
  try {
    const user = await currentUser();

    if (!user?.id) {
      return new NextResponse("Unauthorized", { status: 401 });
    }

    return NextResponse.json(user);
  } catch (error) {
    console.log("ERROR in [/v1/me]", error);
    return new NextResponse("Internal Error", { status: 500 });
  }
};