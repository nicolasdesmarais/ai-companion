import LandingFooter from "@/components/landing-footer";
import LandingNav from "@/components/landing-nav";
import { ArrowRight } from "lucide-react";
import Image from "next/image";
import Link from "next/link";

const Solutions = () => {
  return (
    <div className="bg-white flex flex-col text-navy">
      <LandingNav />

      <div className="py-10 mt-10 flex flex-col items-center bg-[#F8F8F8] ">
        <div className="lg:w-[1140px] mx-4">
          <h2 className="text-5xl font-bold">Solutions</h2>
          <div className="lg:w-[700px] mt-10 text-lg">
            Streamline your daily tasks with the help of your own custom-fit AI
            app. Companies from all industries can use AI to boost productivity
            and efficiency.
          </div>
        </div>
      </div>

      <div className="flex flex-col items-center mt-20 mb-12 mx-4 lg:mx-0">
        <div className="flex flex-col-reverse lg:flex-row justify-between">
          <div className="lg:w-[400px] gap-8 flex flex-col mt-10 lg:mt-0">
            <svg
              width="72"
              height="72"
              viewBox="0 0 72 73"
              fill="none"
              xmlns="http://www.w3.org/2000/svg"
            >
              <path
                d="M40.2301 37.6501C46.1101 37.6501 50.8501 42.4201 50.8501 48.2701V66.5401H29.5801V48.2701C29.5801 42.3901 34.3501 37.6501 40.2001 37.6501H40.2301Z"
                fill="#ABE7FF"
              />
              <path
                d="M60.06 48.54C63.42 48.54 66.12 51.27 66.12 54.6V66.54H54V54.6C54 51.24 56.73 48.54 60.06 48.54Z"
                fill="#ABE7FF"
              />
              <path
                d="M17.9999 48.54C21.3599 48.54 24.0599 51.27 24.0599 54.6V66.54H11.9399V54.6C11.9399 51.24 14.6699 48.54 17.9999 48.54Z"
                fill="#ABE7FF"
              />
              <path
                d="M6 52.83V66.54H9.42V52.26C9.78 48.63 12.72 45.99 16.29 45.99H16.35C16.44 44.82 16.62 43.68 16.89 42.57C11.04 42.24 6 46.98 6 52.83Z"
                fill="#011B58"
              />
              <path
                d="M36 6.54004C29.37 6.54004 24 11.91 24 18.54C24 25.17 29.37 30.54 36 30.54C42.63 30.54 48 25.17 48 18.54C48 11.91 42.63 6.54004 36 6.54004ZM36 27.12C31.26 27.12 27.42 23.28 27.42 18.54C27.42 13.8 31.26 9.96004 36 9.96004C40.74 9.96004 44.58 13.8 44.58 18.54C44.58 23.28 40.74 27.12 36 27.12Z"
                fill="#011B58"
              />
              <path
                d="M16.5 24.54C12.36 24.54 9 27.9 9 32.04C9 36.18 12.36 39.54 16.5 39.54C20.64 39.54 24 36.18 24 32.04C24 27.9 20.64 24.54 16.5 24.54ZM16.5 35.79C14.43 35.79 12.75 34.11 12.75 32.04C12.75 29.97 14.43 28.29 16.5 28.29C18.57 28.29 20.25 29.97 20.25 32.04C20.25 34.11 18.57 35.79 16.5 35.79Z"
                fill="#011B58"
              />
              <path
                d="M55.5 24.54C51.36 24.54 48 27.9 48 32.04C48 36.18 51.36 39.54 55.5 39.54C59.64 39.54 63 36.18 63 32.04C63 27.9 59.64 24.54 55.5 24.54ZM55.5 35.79C53.43 35.79 51.75 34.11 51.75 32.04C51.75 29.97 53.43 28.29 55.5 28.29C57.57 28.29 59.25 29.97 59.25 32.04C59.25 34.11 57.57 35.79 55.5 35.79Z"
                fill="#011B58"
              />
              <path
                d="M35.9999 33.54C27.8099 33.54 21.1499 40.2 21.1499 48.39V66.54H24.4499V47.76C24.8099 41.61 29.8799 36.84 35.9999 36.84H36.6299C42.7799 37.2 47.5499 42.27 47.5499 48.39V66.54H50.8499V48.39C50.8499 40.2 44.1899 33.54 35.9999 33.54Z"
                fill="#011B58"
              />
              <path
                d="M55.5 42.54C55.77 43.65 55.95 44.79 56.04 45.96H56.67C60.3 46.32 62.94 49.26 62.94 52.83V66.54H66.36V52.83C66.36 46.95 61.38 42.24 55.47 42.57L55.5 42.54Z"
                fill="#011B58"
              />
            </svg>
            <div className="text-3xl font-bold ">AI for Human Resources</div>
            <div>
              Automate HR workflows while improving employee experience.
            </div>
            <ul className="list-disc gap-4 flex flex-col ml-4">
              <li>
                <b>Automate onboarding:</b> Load your company&apos;s process and
                onboarding training information into your very own AI app for a
                consistent employee experience.
              </li>
              <li>
                <b>Analyze employee engagement:</b> One organization used their
                AI to analyze their employees&apos; culture amp data to better
                understand the results, quickly.
              </li>
              <li>
                <b>Provide 24/7 access to HR support:</b> Quickly respond to
                employees&apos; HR-related queries, regardless of time zone.
              </li>
            </ul>
            <Link href="/sign-up" className="py-2 text-royal">
              Start creating
              <ArrowRight className="inline-block w-4 h-4 ml-2" />
            </Link>
          </div>
          <div className="pl-16">
            <Image
              src="/hr-chat.png"
              alt="AI HR Chat Screenshot"
              width="663"
              height="627"
            />
          </div>
        </div>
      </div>

      <div className="flex flex-col items-center my-12 py-16 px-4 bg-[#F8F8F8]">
        <div className="flex flex-col lg:flex-row">
          <div className="">
            <Image
              src="/marketing-chat.png"
              alt="AI Marketing Chat Screenshot"
              width="663"
              height="627"
            />
          </div>
          <div className="lg:w-[460px] gap-8 flex flex-col lg:ml-20 mt-10 lg:mt-0">
            <svg
              width="80"
              height="73"
              viewBox="0 0 80 73"
              fill="none"
              xmlns="http://www.w3.org/2000/svg"
            >
              <path
                d="M63.205 15.571L39.7549 31.651V42.031L63.205 57.991C75.8017 46.291 75.8017 27.301 63.205 15.601V15.571Z"
                fill="#ABE7FF"
              />
              <path
                d="M49.9178 33.2711L49.2271 33.0911V18.8411C49.2271 18.0611 48.5364 17.4611 47.7142 17.4611H45.6421C38.0118 24.1211 21.5342 26.3411 20.8106 26.4311H10.8451C8.64151 26.4311 6.86548 28.0811 6.86548 30.0611V43.5011C6.86548 45.5111 8.64151 47.1311 10.8451 47.1311H14.0683L16.535 58.3511H19.7581V47.0111L20.8106 47.1611C21.5013 47.2511 38.0118 49.4711 45.6421 56.1311H47.7142C48.5364 56.1311 49.2271 55.5011 49.2271 54.7511V40.5011L49.9178 40.3511C51.6609 39.9311 52.9107 38.4911 52.9107 36.8411C52.9107 35.1911 51.6938 33.7511 49.9178 33.3311V33.2711ZM21.5999 44.3711H9.89131V29.2211H21.5999V44.3711ZM46.1684 52.8011L44.6883 51.9011C41.3007 49.7711 36.762 47.9411 31.3023 46.4711C29.4276 45.9611 27.4214 45.5111 25.3822 45.0911L24.6587 44.9411V28.5911L25.3822 28.4411C27.4214 28.0511 29.3947 27.5711 31.3023 27.0611C36.7949 25.5911 41.3007 23.7611 44.6883 21.6311L46.1684 20.7011V52.7411V52.8011Z"
                fill="#011B58"
              />
            </svg>

            <div className="text-3xl font-bold lg:w-[300px]">
              AI for Marketing
            </div>
            <div>
              Reach the right audience with the right message, every time.
            </div>
            <ul className="list-disc gap-4 flex flex-col ml-4">
              <li>
                <b>Gain in-depth persona insights:</b> Feed your AI your
                customer data to create more targeted and effective marketing
                campaigns.
              </li>
              <li>
                <b>Personalize content:</b> Quickly adjust campaign messaging
                and copy formats to best suit your target audience and marketing
                channel.
              </li>
              <li>
                <b>Achieve consistency:</b> Augment the AI with your brand and
                content guidelines to ensure your tone, brand voice, and
                nomenclature are on point.
              </li>
            </ul>
            <Link href="/public" className="py-2 text-royal">
              Start using
              <ArrowRight className="inline-block w-4 h-4 ml-2" />
            </Link>
          </div>
        </div>
      </div>

      <div className="flex flex-col items-center mt-20 mb-12 mx-4">
        <div className="flex flex-col-reverse lg:flex-row">
          <div className="lg:w-[460px] gap-8 flex flex-col mt-10 lg:mt-0">
            <svg
              width="72"
              height="73"
              viewBox="0 0 72 73"
              fill="none"
              xmlns="http://www.w3.org/2000/svg"
            >
              <path
                d="M42.3301 35.237H29.2501C27.6301 34.907 26.5201 33.617 26.5201 32.087C26.5201 30.347 27.9301 28.937 29.6701 28.937H45.4501V25.787H37.5601V19.457H34.4101V25.787H29.6701C26.1901 25.787 23.3401 28.607 23.3401 32.117C23.3401 35.627 26.1601 38.447 29.6701 38.447H42.7201C44.3701 38.777 45.4501 40.067 45.4501 41.597C45.4501 43.337 44.0401 44.747 42.3001 44.747H26.5201V47.897H34.4101V54.227H37.5601V47.897H42.3001C45.7801 47.897 48.6301 45.077 48.6301 41.567C48.6301 38.057 45.8101 35.237 42.3001 35.237H42.3301Z"
                fill="#011B58"
              />
              <path
                d="M36 66.827C19.44 66.827 6 53.387 6 36.827C6 20.267 19.44 6.82703 36 6.82703C43.29 6.82703 49.95 9.40703 55.14 13.727L55.86 14.357L53.16 20.057C48.81 15.587 42.72 12.827 35.97 12.827C22.71 12.827 11.97 23.567 11.97 36.827C11.97 50.087 22.71 60.827 35.97 60.827C49.23 60.827 59.55 50.477 59.97 37.577V36.827H52.47L60.6 19.667C63.99 24.527 65.97 30.437 65.97 36.827C65.97 53.387 52.53 66.827 35.97 66.827H36Z"
                fill="#ABE7FF"
              />
            </svg>
            <div className="text-3xl font-bold lg:w-[300px]">
              AI for Sales Teams
            </div>
            <div>Sell faster with your own AI application.</div>
            <ul className="list-disc gap-4 flex flex-col ml-4">
              <li>
                <b>Access up-to-date product information:</b> Gain access to
                company&apos;s products and services within seconds to help you
                answer customer queries accurately.
              </li>
              <li>
                <b>Streamline sales support:</b> Use your app to assist with
                more complex technical aspects of your products.
              </li>
              <li>
                <b>Write customer communications:</b> Use your AI to help you
                craft on-brand emails that hit the right tone for your specific
                message and customer.
              </li>
            </ul>
            <Link href="/sign-up" className="py-2 text-royal">
              Start creating
              <ArrowRight className="inline-block w-4 h-4 ml-2" />
            </Link>
          </div>
          <div className="">
            <Image
              src="/sales-chat.png"
              alt="AI Sales Chat"
              width="663"
              height="627"
            />
          </div>
        </div>
      </div>

      <div className="flex flex-col items-center my-12 py-16 px-4 bg-[#F8F8F8]">
        <div className="flex flex-col lg:flex-row">
          <div className="">
            <Image
              src="/legal-chat.png"
              alt="AI Legal Chat Screenshot"
              width="663"
              height="627"
            />
          </div>
          <div className="lg:w-[460px] gap-8 flex flex-col lg:ml-20 mt-10 lg:mt-0">
            <svg
              width="72"
              height="73"
              viewBox="0 0 72 73"
              fill="none"
              xmlns="http://www.w3.org/2000/svg"
            >
              <g clip-path="url(#clip0_1519_997)">
                <path
                  d="M63.2101 15.1101L53.2501 18.4101L36.7501 12.9201V9.50006H33.7501V12.9201L17.2501 18.4101L7.29011 15.1101L6.36011 17.9601L15.4801 20.9901H19.0201L33.7501 16.1601V20.3901V20.9901V60.5001H21.7501V63.5001H48.7501V60.5001H36.7501V20.9901V20.3901V16.1601L51.4801 20.9901H55.0201L64.1401 17.9601L63.2101 15.1101Z"
                  fill="#011B58"
                />
                <path
                  d="M17.25 18.2601L6 44.3301C9.06 47.1801 13.02 48.7701 17.25 48.7701C21.48 48.7701 25.44 47.2101 28.5 44.3301L17.25 18.2601Z"
                  fill="#ABE7FF"
                />
                <path
                  d="M53.25 18.2601L42 44.3301C45.06 47.1801 49.02 48.7701 53.25 48.7701C57.48 48.7701 61.44 47.2101 64.5 44.3301L53.25 18.2601Z"
                  fill="#ABE7FF"
                />
              </g>
              <defs>
                <clipPath id="clip0_1519_997">
                  <rect
                    width="58.5"
                    height="54"
                    fill="white"
                    transform="translate(6 9.50006)"
                  />
                </clipPath>
              </defs>
            </svg>
            <div className="text-3xl font-bold lg:w-[360px]">
              AI for Legal Teams
            </div>
            <div>Go from reactive to proactive.</div>
            <ul className="list-disc gap-4 flex flex-col ml-4 mt-6">
              <li>
                <b>Create templates:</b> Easily create templates for commonly
                used documents such as non-disclosure agreements or employment
                contracts.
              </li>
              <li>
                <b>Complete compliance checks:</b> Quickly access information on
                jurisdiction-specific laws or quickly scan documents for
                potential compliance issues.
              </li>
              <li>
                <b>Complete risk assessments:</b> Highlight potential legal
                risks in business transactions like mergers, acquisitions, and
                partnerships.
              </li>
            </ul>
            <Link href="/sign-up" className="py-2 mt-20 text-royal">
              Start creating
              <ArrowRight className="inline-block w-4 h-4 ml-2" />
            </Link>
          </div>
        </div>
      </div>

      <LandingFooter />
    </div>
  );
};

export default Solutions;
