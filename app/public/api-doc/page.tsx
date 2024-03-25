import { PaywallBanner } from "@/components/paywall-banner";
import ReactSwagger from "./components/react-swagger";

export default async function IndexPage() {
  return (
    <section className="container">
      <PaywallBanner className="mt-3" />
      <ReactSwagger />
    </section>
  );
}
