import LandingFAQ from "@/components/landing-faq";
import LandingFooter from "@/components/landing-footer";
import LandingNav from "@/components/landing-nav";
import LandingSupport from "@/components/landing-support";
import { Check } from "lucide-react";
import Link from "next/link";

const plans = [
  {
    name: "$10/month",
    price: "10",
    data: "3GB/mo",
  },
  {
    name: "$100/month",
    price: "100",
    data: "30GB/mo",
  },
  {
    name: "Custom",
    price: "Custom",
    data: "> 30GB/mo",
    custom: true,
    tuning: true,
  },
];

const LandingFeatures = () => {
  return (
    <div className="bg-white flex flex-col text-navy">
      <LandingNav />

      <div className="py-10 mt-10 flex flex-col items-center bg-[#F8F8F8]">
        <div className="lg:w-[1140px] mx-4">
          <h2 className="text-5xl font-bold">Pricing</h2>
        </div>
      </div>

      <div className="flex items-center justify-center my-16">
        <div className="lg:w-[1100px] mx-4 flex flex-col items-center">
          <h2 className="text-3xl font-bold text-center mb-8">
            Choose the plan that&apos;s right for you
          </h2>
          <Link href="/sign-up" className="px-8 py-2 bg-sky">
            Sign up
          </Link>
          <div className="flex mt-16 justify-center">
            <div className="grid grid-cols-2 w-1/3">
              <div></div>
              <div className="font-bold border-b p-8 h-20">Free</div>
              <div>Data usage</div>
              <div className="font-bold p-8 h-20">200MB/mo</div>
              <div>Use and create unlimited AIs</div>
              <div className="font-bold">
                <Check />
              </div>
              <div>Share AIs easily via email or generated link</div>
              <div className="font-bold">
                <Check />
              </div>
              <div>Multiple LLMs to choose from for each AI</div>
              <div className="font-bold">
                <Check />
              </div>
              <div>All proprietary data is kept secure and private</div>
              <div className="font-bold">
                <Check />
              </div>
              <div>AI evaluation and custom configuration</div>
              <div></div>
              <div>
                Advanced integrations, i.e. LLM fine tuning and private
                deployment
              </div>
              <div></div>
            </div>
            <div className="grid grid-cols-3 shadow-lg">
              {plans.map((plan) => (
                <div
                  key={`plan-${plan.name}`}
                  className="font-bold border-b p-8 h-20"
                >
                  {plan.name}
                </div>
              ))}
              {plans.map((plan) => (
                <div key={`data-${plan.name}`} className="font-bold p-8 h-20">
                  {plan.data}
                </div>
              ))}
              {plans.map((plan) => (
                <div key={`create-${plan.name}`} className="font-bold">
                  <Check />
                </div>
              ))}
              {plans.map((plan) => (
                <div key={`share-${plan.name}`} className="font-bold">
                  <Check />
                </div>
              ))}
              {plans.map((plan) => (
                <div key={`llms-${plan.name}`} className="font-bold">
                  <Check />
                </div>
              ))}
              {plans.map((plan) => (
                <div key={`secure-${plan.name}`} className="font-bold">
                  <Check />
                </div>
              ))}
              {plans.map((plan) => (
                <div key={`custom-${plan.name}`} className="font-bold">
                  {plan.custom ? <Check /> : null}
                </div>
              ))}
              {plans.map((plan) => (
                <div key={`tuning-${plan.name}`} className="font-bold">
                  {plan.tuning ? <Check /> : null}
                </div>
              ))}
            </div>
          </div>
        </div>
      </div>

      <LandingFAQ />

      <LandingSupport />

      <LandingFooter />
    </div>
  );
};

export default LandingFeatures;
