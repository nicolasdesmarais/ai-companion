import { auth } from "@clerk/nextjs";
import { NextResponse } from "next/server";
import axios from "axios";

export const maxDuration = 300;

export async function POST(req: Request) {
  try {
    const { userId } = auth();
    const body = await req.json();
    const { prompt, imgUrl } = body;

    if (!userId) {
      return new NextResponse("Unauthorized", { status: 401 });
    }

    const talk = await axios.post(
      "https://api.d-id.com/talks",
      {
        source_url: imgUrl,
        script: {
          type: "text",
          input: prompt,
          provider: {
            type: "microsoft",
            voice_id: "en-US-JennyNeural",
            voice_config: {
              style: "Cheerful",
            },
          },
        },
      },
      {
        headers: {
          Authorization: `Basic ${process.env.D_ID_KEY}`,
        },
      }
    );

    if (talk.status === 201) {
      return new NextResponse(JSON.stringify(talk.data), { status: 200 });
    }
    return new NextResponse("Talk Error", { status: 500 });
  } catch (error) {
    console.log("[TALK_ERROR]", error);
    return new NextResponse("Internal Error", { status: 500 });
  }
}
