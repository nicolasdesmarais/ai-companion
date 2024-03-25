"use client";
import { cn } from "@/src/lib/utils";
import { Check, Minus, Plus, X } from "lucide-react";
import Link from "next/link";
import { useState } from "react";

const allPlans = [
  {
    name: "Free",
    data: "200MB/mo",
  },
  {
    name: "$10/month",
    data: "3GB/mo",
  },
  {
    name: "$100/month",
    data: "30GB/mo",
  },
  {
    name: "Custom",
    data: "> 30GB/mo",
    custom: true,
    tuning: true,
  },
];

const plans = allPlans.slice(1);

interface Props {
  className?: string;
}

const LandingPricing = ({ className }: Props) => {
  const [visible, setVisible] = useState([true]);
  const toggle = (index: number) => {
    setVisible((prev) => {
      const newVisible = [...prev];
      newVisible[index] = !newVisible[index];
      return newVisible;
    });
  };
  return (
    <div
      className={cn(
        "flex flex-col items-center mt-20 mb-12 gap-8 mx-4",
        className
      )}
    >
      <div className="flex flex-col justify-center items-center">
        <div className="flex items-center justify-center my-16">
          <div className="lg:w-[1100px] mx-4 flex flex-col items-center">
            <h2 className="text-3xl font-bold text-center mb-8">
              Choose the plan that&apos;s right for you
            </h2>
            <Link href="/sign-up" className="px-8 py-2 bg-sky">
              Sign up
            </Link>
            <div className="hidden lg:flex mt-16 justify-center">
              <div className="grid grid-cols-2 w-1/3 bg-[#F8F8F8] pl-4">
                <div className="border-b border-[#DDDDDD] p-8 h-20"></div>
                <div className="font-bold border-b border-[#DDDDDD] p-8 h-20 text-center">
                  Free
                </div>
                <div className="border-b border-[#DDDDDD] flex items-center min-w-[120px]">
                  Data usage
                </div>
                <div className="font-bold h-20 text-center border-b border-[#DDDDDD] flex items-center justify-center">
                  200MB/mo
                </div>
                <div className="border-b border-[#DDDDDD] flex items-center">
                  Use and create unlimited AIs
                </div>
                <div className="font-bold h-28 text-center border-b border-[#DDDDDD] flex items-center justify-center">
                  <Check className="h-10 w-10" />
                </div>
                <div className="border-b border-[#DDDDDD] flex items-center">
                  Share AIs easily via email or generated link
                </div>
                <div className="font-bold h-28 border-b border-[#DDDDDD] flex items-center justify-center">
                  <Check className="h-10 w-10" />
                </div>
                <div className="border-b h-28 border-[#DDDDDD] flex items-center">
                  Multiple LLMs to choose from for each AI
                </div>
                <div className="font-bold h-28 border-b border-[#DDDDDD] flex items-center justify-center">
                  <Check className="h-10 w-10" />
                </div>
                <div className=" border-b h-28 border-[#DDDDDD] flex items-center">
                  All proprietary data is kept secure and private
                </div>
                <div className="font-bold h-28 border-b border-[#DDDDDD] flex items-center justify-center">
                  <Check className="h-10 w-10" />
                </div>
                <div className="border-b h-28 border-[#DDDDDD] flex items-center">
                  AI evaluation and custom configuration
                </div>
                <div className="border-b h-28 border-[#DDDDDD] flex items-center justify-center">
                  <X className="h-10 w-10" />
                </div>
                <div className="h-28 flex items-center">
                  Advanced integrations, i.e. LLM fine tuning and private
                  deployment
                </div>
                <div className="border-b h-28 border-[#DDDDDD] flex items-center justify-center">
                  <X className="h-10 w-10" />
                </div>
              </div>
              <div className="grid grid-cols-3 shadow-lg text-center pr-4">
                {plans.map((plan) => (
                  <div
                    key={`plan-${plan.name}`}
                    className="font-bold border-b border-[#DDDDDD] p-8 h-20"
                  >
                    {plan.name}
                  </div>
                ))}
                {plans.map((plan) => (
                  <div
                    key={`data-${plan.name}`}
                    className="font-bold border-b border-[#DDDDDD] flex items-center justify-center h-20"
                  >
                    {plan.data}
                  </div>
                ))}
                {plans.map((plan) => (
                  <div
                    key={`create-${plan.name}`}
                    className="font-bold border-b border-[#DDDDDD] h-28 flex items-center justify-center"
                  >
                    <Check className="h-10 w-10" />
                  </div>
                ))}
                {plans.map((plan) => (
                  <div
                    key={`share-${plan.name}`}
                    className="font-bold border-b border-[#DDDDDD] h-28 flex items-center justify-center"
                  >
                    <Check className="h-10 w-10" />
                  </div>
                ))}
                {plans.map((plan) => (
                  <div
                    key={`llms-${plan.name}`}
                    className="font-bold border-b border-[#DDDDDD] h-28 flex items-center justify-center"
                  >
                    <Check className="h-10 w-10" />
                  </div>
                ))}
                {plans.map((plan) => (
                  <div
                    key={`secure-${plan.name}`}
                    className="font-bold border-b border-[#DDDDDD] h-28 flex items-center justify-center"
                  >
                    <Check className="h-10 w-10" />
                  </div>
                ))}
                {plans.map((plan) => (
                  <div
                    key={`custom-${plan.name}`}
                    className="font-bold border-b border-[#DDDDDD] h-28 flex items-center justify-center"
                  >
                    {plan.custom ? (
                      <Check className="h-10 w-10" />
                    ) : (
                      <X className="h-10 w-10" />
                    )}
                  </div>
                ))}
                {plans.map((plan) => (
                  <div
                    key={`tuning-${plan.name}`}
                    className="font-bold h-28 flex items-center justify-center"
                  >
                    {plan.tuning ? (
                      <Check className="h-10 w-10" />
                    ) : (
                      <X className="h-10 w-10" />
                    )}
                  </div>
                ))}
              </div>
            </div>

            <div className="lg:hidden mt-16 w-full shadow-lg">
              {allPlans.map((plan, index) => (
                <div
                  key={`mobile-plan-${index}`}
                  className={cn(
                    "w-full space-y-4 px-8 pt-8",
                    visible[index] ? "bg-[#FAF7F7] pb-8" : ""
                  )}
                >
                  <div
                    className={cn(
                      "flex justify-between cursor-pointer pb-8",
                      visible[index] ||
                        allPlans.length - 1 === index ||
                        visible[index + 1]
                        ? ""
                        : "border-b border-[#DDDDDD]"
                    )}
                    onClick={() => toggle(index)}
                  >
                    <div>
                      <h3 className="text-xl font-bold mb-2">{plan.data}</h3>
                      <h3 className="text-xl font-bold">{plan.name}</h3>
                    </div>
                    {visible[index] ? (
                      <Minus className="w-6 h-6" />
                    ) : (
                      <Plus className="w-6 h-6" />
                    )}
                  </div>
                  {visible[index] && (
                    <div className="px-2 gap-4 flex flex-col  ">
                      <div className="flex gap-4">
                        <div className="h-6 w-6">
                          <Check className="h-6 w-6" />
                        </div>
                        <div>Use and create unlimited AIs</div>
                      </div>

                      <div className="flex gap-4">
                        <div className="h-6 w-6">
                          <Check className="h-6 w-6" />
                        </div>
                        <div>Share AIs easily via email or generated link</div>
                      </div>

                      <div className="flex gap-4">
                        <div className="h-6 w-6">
                          <Check className="h-6 w-6" />
                        </div>
                        <div>Multiple LLMs to choose from for each AI</div>
                      </div>

                      <div className="flex gap-4">
                        <div className="h-6 w-6">
                          <Check className="h-6 w-6" />
                        </div>
                        <div>
                          All proprietary data is kept secure and private
                        </div>
                      </div>

                      <div className="flex gap-4">
                        <div className="h-6 w-6">
                          {plan.custom ? (
                            <Check className="h-6 w-6" />
                          ) : (
                            <X className="h-6 w-6" />
                          )}
                        </div>
                        <div>AI evaluation and custom configuration</div>
                      </div>

                      <div className="flex gap-4">
                        <div className="h-6 w-6">
                          {plan.tuning ? (
                            <Check className="h-6 w-6" />
                          ) : (
                            <X className="h-6 w-6" />
                          )}
                        </div>
                        <div>
                          Advanced integrations, i.e. LLM fine tuning and
                          private deployment
                        </div>
                      </div>
                    </div>
                  )}
                </div>
              ))}
            </div>
          </div>
        </div>
      </div>
    </div>
  );
};

export default LandingPricing;
