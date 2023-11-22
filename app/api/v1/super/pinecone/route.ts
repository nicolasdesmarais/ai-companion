import { auth } from "@clerk/nextjs";
import { NextResponse } from "next/server";
import { Pinecone } from "@pinecone-database/pinecone";
import axios from "axios";

const getMetics = async (indexName: string) => {
  // only available for enterprise plan :'(
  const resp = await axios.get(
    `https://metrics.${indexName}.pinecone.io/metrics`,
    {
      headers: { Authorization: `Bearer ${process.env.PINECONE_API_KEY}` },
    }
  );
  console.log(resp.data);
};

const pinecone = new Pinecone();

export async function GET(req: Request) {
  try {
    const { userId, sessionClaims } = await auth();
    if (!userId || !(sessionClaims?.meta as any)?.superuser) {
      return new NextResponse("Unauthorized", { status: 401 });
    }

    const indexNames = await pinecone.listIndexes();
    const indexes = await Promise.all(
      indexNames.map(async ({ name }) => {
        const indexDescription = await pinecone.describeIndex(name);
        const index = pinecone.Index(name);
        const indexStats = await index.describeIndexStats();
        const isCurrent = process.env.PINECONE_INDEX === name;
        return { indexDescription, indexStats, isCurrent };
      })
    );
    const collectionNames = await pinecone.listCollections();
    const collections = await Promise.all(
      collectionNames.map(async ({ name }) => {
        const description = await pinecone.describeCollection(name);
        return description;
      })
    );

    return NextResponse.json({ indexes, collections });
  } catch (error) {
    console.error("[GET v1/super/pinecone]", error);
    return new NextResponse("Internal Error", { status: 500 });
  }
}
