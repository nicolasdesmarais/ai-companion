import { isSuperuser } from "@/src/lib/utils";
import { auth } from "@clerk/nextjs";
import { NextResponse } from "next/server";
import { Pinecone } from "@pinecone-database/pinecone";

const pinecone = new Pinecone();

export async function GET(
  req: Request,
  { params: { indexName } }: { params: { indexName: string } }
) {
  try {
    const { userId } = await auth();
    if (!userId || !isSuperuser(userId)) {
      return new NextResponse("Unauthorized", { status: 401 });
    }

    const result = await pinecone.createCollection({
      name: `${indexName}-${new Date().toISOString()}`
        .toLowerCase()
        .replace(/:/g, "-"),
      source: indexName,
    });
    console.log(result);
    return NextResponse.json("ok");
  } catch (error) {
    console.error("[GET v1/super/data-sources]", error);
    return new NextResponse("Internal Error", { status: 500 });
  }
}
