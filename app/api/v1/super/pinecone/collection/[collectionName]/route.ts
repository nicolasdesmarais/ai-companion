import { isSuperuser } from "@/src/lib/utils";
import { auth } from "@clerk/nextjs";
import { NextResponse } from "next/server";
import { Pinecone } from "@pinecone-database/pinecone";

const pinecone = new Pinecone();

export async function DELETE(
  req: Request,
  { params: { collectionName } }: { params: { collectionName: string } }
) {
  try {
    const { userId } = await auth();
    if (!userId || !isSuperuser(userId)) {
      return new NextResponse("Unauthorized", { status: 401 });
    }
    await pinecone.deleteCollection(collectionName);
    return NextResponse.json("ok");
  } catch (error) {
    console.error(
      "[DELETE v1/super/pinecone/collection/[collectionName]]",
      error
    );
    return new NextResponse("Internal Error", { status: 500 });
  }
}
