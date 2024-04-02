import { PaywallBanner } from "@/components/paywall-banner";
import { auth } from "@clerk/nextjs";
import { redirect } from "next/navigation";
import ReactSwagger from "./components/react-swagger";

export default async function IndexPage() {
  const { userId } = auth();

  if (userId) {
    return redirect("/api-doc");
  }

  return (
    <section className="container">
      <PaywallBanner className="mt-3" />
      <ReactSwagger />
    </section>
  );
}
