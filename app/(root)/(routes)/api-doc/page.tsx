import { getApiDocs } from "@/src/lib/swagger";
import ReactSwagger from "./components/react-swagger";

export default async function IndexPage() {
  const spec = await getApiDocs();
  return (
    <section className="container">
      <ReactSwagger spec={spec} />
    </section>
  );
}
