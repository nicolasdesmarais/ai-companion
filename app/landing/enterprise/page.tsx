import LandingFooter from "@/components/landing-footer";
import LandingNav from "@/components/landing-nav";
import Image from "next/image";

const LandingFeatures = () => {
  return (
    <div className="bg-white flex flex-col text-navy">
      <LandingNav />

      <div className="py-20 flex flex-col items-center bg-navy text-white">
        <div className="lg:w-[1140px] mx-4 flex justify-between">
          <div className="flex flex-col justify-center">
            <h2 className="text-5xl font-bold">AI for the Enterprise</h2>
            <div className="lg:w-[300px] mt-10 text-lg">
              For companies looking to build AI-driven solutions that solve
              complex supply chain problems.
            </div>
          </div>
          <div className="lg:bg-gradient1 lg:w-[572px] lg:h-[544px] flex items-center">
            <Image
              src="/ship.png"
              alt="Container Ship"
              width="451"
              height="422"
              className="-ml-10"
            />
          </div>
        </div>
      </div>

      <div className="py-16 flex flex-col items-center bg-[#F8F8F8]">
        <div className="lg:w-[1140px] mx-4">
          <svg
            width="71"
            height="73"
            viewBox="0 0 71 73"
            fill="none"
            xmlns="http://www.w3.org/2000/svg"
          >
            <path
              d="M27.6721 23.021L27.9929 22.121C29.2763 18.401 28.3429 14.201 25.6304 11.411C23.5013 9.22103 20.5554 8.17103 17.6388 8.44103L23.0638 14.051C25.5429 16.601 25.5429 20.771 23.0638 23.321C20.5846 25.871 16.5596 25.871 14.0513 23.321L8.5971 17.711C8.3346 20.711 9.35543 23.771 11.4846 25.931C14.1971 28.721 18.2804 29.681 21.8971 28.361L22.7721 28.031L57.8596 64.121L62.7304 59.111L27.6721 23.021Z"
              fill="#011B58"
            />
            <path
              d="M56.9555 22.0911L61.8555 13.0011L58.793 9.85107L49.9555 14.8911L48.8471 20.6211L27.7305 42.3411L30.268 44.9511L51.3846 23.2611L56.9555 22.0911Z"
              fill="#ABE7FF"
            />
            <path
              d="M10.3763 62.8011C11.6888 64.1511 13.9346 64.1511 15.2471 62.8011L32.9513 44.5911L28.0804 39.5811L10.0846 58.1211C9.00544 59.4711 9.09294 61.5111 10.3471 62.8311L10.3763 62.8011Z"
              fill="#011B58"
            />
          </svg>
          <h2 className="text-4xl font-bold mt-4">
            Building AI for the real world
          </h2>
          <div className="lg:w-[900px] mt-10 text-lg">
            In partnership with IVADO Labs, our unique team of world-class
            researchers, data scientists, and strategists will help you realize
            the transformational potential of AI for your business.
          </div>
          <div className="lg:w-[900px] mt-10 text-lg">
            Founded in 2017, IVADO Labs is a leading AI solution provider with a
            mission to take advanced AI technologies out of the lab and apply
            them in the real world.
          </div>
        </div>
      </div>

      <div className="py-16 flex flex-col items-center">
        <div className="lg:w-[1140px] mx-4">
          <svg
            width="71"
            height="66"
            viewBox="0 0 71 66"
            fill="none"
            xmlns="http://www.w3.org/2000/svg"
          >
            <path
              d="M65.1507 25.6545V22.8208H59.0104V12.9027C59.0104 12.1093 58.3349 11.4858 57.4753 11.4858H46.7297V5.81836H43.6595V11.4858H28.3086V5.81836H25.2384V11.4858H14.4928C13.6332 11.4858 12.9577 12.1093 12.9577 12.9027V22.8208H6.81738V25.6545H12.9577V39.8232H6.81738V42.657H12.9577V52.5751C12.9577 53.3685 13.6332 53.9919 14.4928 53.9919H25.2384V59.6594H28.3086V53.9919H43.6595V59.6594H46.7297V53.9919H57.4753C58.3349 53.9919 59.0104 53.3685 59.0104 52.5751V42.657H65.1507V39.8232H59.0104V25.6545H65.1507ZM55.9402 51.1582H16.0279V14.3196H55.9402V51.1582Z"
              fill="#011B58"
            />
            <path
              fill-rule="evenodd"
              clip-rule="evenodd"
              d="M51.3357 18.5698H20.6338V46.9072H51.3357V18.5698ZM29.5717 39.9734H26.2076L31.3335 26.2684H35.3791L40.4978 39.9734H37.1337L36.0343 36.8483H30.6732L29.5717 39.9734ZM33.4143 29.4002L35.2387 34.5864H31.4704L33.2983 29.4002H33.4143ZM44.8118 26.2684V39.9734H41.6725V26.2684H44.8118Z"
              fill="#ABE7FF"
            />
          </svg>

          <h2 className="text-4xl font-bold mt-4">
            Complex problems call for custom solutions
          </h2>
          <div className="lg:w-[900px] mt-10 text-lg gap-4 flex flex-col">
            Enterprises can leverage our expertise to customize AppDirect AI,
            aligning with their specific data governance and organizational
            objectives, including:
            <div className="font-bold">AI Configuration:</div>
            <ul className="list-disc gap-4 ml-8">
              <li>Advanced data assessment to optimize AI performance</li>
              <li>LLM evaluation to ensure the best fit for your needs</li>
              <li>
                Advanced integrations for seamless workflow implementation
              </li>
            </ul>
            <div className="font-bold">Advanced Features:</div>
            <ul className="list-disc gap-4 ml-8">
              <li>Private LLM deployment for enhanced security and control</li>
              <li>Fine-tuning for precise AI model customization</li>
              <li>Workflows to streamline AI-driven processes</li>
            </ul>
          </div>
        </div>
      </div>

      <div className="my-16 flex items-center relative">
        <div className="absolute top-0 w-full flex justify-center">
          <div className="lg:w-[700px] mt-10 text-lg gap-4 flex flex-col text-center">
            <h2 className="text-4xl font-bold mt-4">Our process</h2>
            We are ready to help you with your own business transformation. It
            all starts with developing solutions that are measurable and
            impactful.
          </div>
        </div>
        <div className="pt-56 bg-[#F8F8F8] flex w-1/2 justify-end">
          <div className="w-56">
            <div className="p-8 flex flex-col gap-4">
              <div className="text-6xl font-bold text-[#F2555A80]">1</div>
              <div className="text-lg font-bold">
                Advisory: Explore the possibilities
              </div>
              <div className="text-sm">
                We&apos;ll help you find the right opportunity for meaningful
                outcomes.
              </div>
            </div>
            <div className="p-8 flex flex-col gap-4">
              <div className="text-6xl font-bold text-[#F2555A80]">3</div>
              <div className="text-lg font-bold">MVP: Test & measure</div>
              <div className="text-sm">
                We work closely with you to develop a minimum viable product
                (MVP) within 3-6 months and prove out the concept.
              </div>
            </div>
          </div>
        </div>
        <div className="pt-56 w-1/2">
          <div className="w-56 p-8 flex flex-col gap-4">
            <div className="text-6xl font-bold text-[#F2555A80]">2</div>
            <div className="text-lg font-bold">
              Advisory: Explore the possibilities
            </div>
            <div className="text-sm">
              We&apos;ll help you find the right opportunity for meaningful
              outcomes.
            </div>
          </div>
          <div className="w-56 p-8 flex flex-col gap-4">
            <div className="text-6xl font-bold text-[#F2555A80]">4</div>
            <div className="text-lg font-bold">MVP: Test & measure</div>
            <div className="text-sm">
              We work closely with you to develop a minimum viable product (MVP)
              within 3-6 months and prove out the concept.
            </div>
          </div>
        </div>
      </div>
      <LandingFooter />
    </div>
  );
};

export default LandingFeatures;
