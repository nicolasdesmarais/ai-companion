import { isSuperuser } from "@/src/lib/utils";
import { auth, redirectToSignIn } from "@clerk/nextjs";
import { Pinecone } from "@pinecone-database/pinecone";
import axios from "axios";
import { redirect } from "next/navigation";

const pinecone = new Pinecone();

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

const SuperPineconePage = async () => {
  const { userId, user } = await auth();

  if (!userId) {
    return redirectToSignIn();
  }

  if (!isSuperuser(userId)) {
    console.log("Superuser attempt", userId);
    return redirect("/");
  }
  if (!process.env.PINECONE_INDEX) {
    return <div>Set PINECONE_INDEX</div>;
  }

  const indexNames = await pinecone.listIndexes();
  const indexes = await Promise.all(
    indexNames.map(async ({ name }) => {
      const indexDescription = await pinecone.describeIndex(name);
      const index = pinecone.Index(name);
      const indexStats = await index.describeIndexStats();
      return { indexDescription, indexStats };
    })
  );
  const collections = await pinecone.listCollections();

  return (
    <div className="h-full p-4 space-y-2">
      <h2 className="text-lg font-medium">Pinecone Indexes</h2>
      <div className="text-muted-foreground text-sm">
        {indexes.map(({ indexDescription, indexStats }: any) => (
          <div key={indexDescription.database.name} className="pt-4">
            <h3 className="text-lg font-medium">
              {indexDescription.database.name}
            </h3>
            <div>Pods: {indexDescription.database.pods}</div>
            <div>Replicas: {indexDescription.database.replicas}</div>
            <div>Shards: {indexDescription.database.shards}</div>
            <div>Pod Type: {indexDescription.database.podType}</div>
            <div>Fullness: {indexStats.indexFullness}</div>
            <div>Total Record Count: {indexStats.totalRecordCount}</div>
          </div>
        ))}
      </div>

      <h3 className="text-lg font-medium">Pinecone Collections</h3>
      <div className="text-muted-foreground text-sm">
        {JSON.stringify(collections)}
      </div>
    </div>
  );
};

export default SuperPineconePage;
