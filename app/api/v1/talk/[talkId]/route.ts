import { auth } from "@clerk/nextjs";
import { NextResponse } from "next/server";
import axios from "axios";

export const maxDuration = 300;

export async function GET(
  request: Request,
  { params: { talkId } }: { params: { talkId: string } }
) {
  try {
    const authentication = await auth();
    const orgId = authentication?.orgId;
    const userId = authentication?.userId;

    if (!userId || !orgId) {
      return new NextResponse("Unauthorized", { status: 401 });
    }

    const talk = await axios.get(`https://api.d-id.com/talks/${talkId}`, {
      headers: {
        Authorization: `Basic ${process.env.D_ID_KEY}`,
      },
    });

    if (talk.status === 200) {
      return new NextResponse(JSON.stringify(talk.data), { status: 200 });
    }
  } catch (error) {
    console.log("[TALK_GET]", error);
    return new NextResponse("Internal Error", { status: 500 });
  }
}
