import BlobAnimation from "@/components/blob-animation";
import LandingCTA from "@/components/landing-cta";
import LandingFooter from "@/components/landing-footer";
import LandingStartChat from "@/components/landing-start-chat";
import LandingTutorials from "@/components/landing-tutorials";
import { ArrowRight } from "lucide-react";
import Link from "next/link";

const LandingHome = () => {
  return (
    <div className="bg-white flex flex-col text-navy">
      <div className="md:h-screen overflow-hidden flex flex-col bg-[#F8F8F8] mb-8 -mt-20 pt-20 min-h-[930px] justify-center">
        <div className="flex flex-col justify-center items-center mx-4">
          <div className="mt-6 md:w-[740px] flex flex-col items-center lg:text-left">
            <h2
              className="me-4 mb-6 font-bold leading-none tracking-tight text-4xl md:text-5xl lg:text-6xl text-center"
              title="AI made simple"
            >
              AppDirect AI Marketplace & Creation Studio
            </h2>
            <div className="mb-6 text-center md:w-[570px]">
              Transform your AI app ideas into reality without needing any
              coding skills. Unlock innovation and productivity for you, your
              team, and your customers.
            </div>
            <div className="flex justify-evenly md:w-[440px]">
              <Link href="/signup" className="px-8 py-2 bg-sky">
                Sign up
              </Link>
              <Link href="/landing/resources#tour" className="px-4 py-2">
                Take a tour
                <ArrowRight className="inline-block w-4 h-4 ml-2" />
              </Link>
            </div>
          </div>
          <div className="my-8 shadow-lg md:w-[800px]">
            <video width="1280" height="720" autoPlay loop muted playsInline>
              <source src="/storyboard720.mp4" type="video/mp4" />
            </video>
          </div>
        </div>
      </div>

      <LandingStartChat />

      <div className="flex flex-col items-center mb-14 mt-20 relative overflow-hidden blob-container">
        <div className=" lg:w-[1110px] px-8 md:px-20 py-16 flex flex-col items-center z-10">
          <h3 className="text-3xl font-bold mb-16 text-center md:w-[680px]">
            Create purpose-built, secure AI apps in minutes—using your data and
            choice of LLM
          </h3>
          <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-8">
            <div className="bg-white px-8 py-16 drop-shadow-lg">
              <h4 className="text-xl font-bold mb-11">Low/no code</h4>
              <div>
                Our low/no code solution empowers you to effortlessly create
                tailor-made AI applications for your business and clientele.
              </div>
            </div>
            <div className="bg-white px-8 py-16 drop-shadow-lg">
              <h4 className="text-xl font-bold mb-4">
                Enterprise-grade governance
              </h4>
              <div>
                Fearlessly build your AI using even the most sensitive data. Our
                admin-first approach puts IT leaders in the driver&apos;s seat,
                ensuring meticulous oversight and control over AI
                implementations.
              </div>
            </div>
            <div className="bg-white px-8 py-16 drop-shadow-lg">
              <h4 className="text-xl font-bold mb-11">Choose your LLM</h4>
              <div>
                App creators can choose the LLM provider most suitable to meet
                their business purpose for every app they create.
              </div>
            </div>
            <div className="bg-white px-8 py-16 drop-shadow-lg">
              <h4 className="text-xl font-bold mb-4">
                Collaborative workspace
              </h4>
              <div>
                Our workspace fosters seamless collaboration, giving teams a
                powerful tool to gain new insights, unlock innovation, and
                increase productivity.
              </div>
            </div>
            <div className="bg-white px-8 py-16 drop-shadow-lg">
              <h4 className="text-xl font-bold mb-11">Your data, your AI</h4>
              <div>
                Train your AI with your proprietary assets to provide results
                that are important to you, then watch it unlock unique insights
                into your business.
              </div>
            </div>
            <div className="bg-navy text-white px-8 py-16 drop-shadow-lg relative">
              <div className="absolute top-0 right-0">
                <svg
                  width="45"
                  height="45"
                  viewBox="0 0 56 57"
                  fill="none"
                  xmlns="http://www.w3.org/2000/svg"
                >
                  <rect
                    x="0.116211"
                    y="0.522156"
                    width="55.6335"
                    height="55.6212"
                    fill="#CDFDDA"
                  />
                  <path
                    d="M18.9391 19.7697L15.712 22.996L13.7217 21.006L20.3465 14.3828L26.9712 21.006L24.9809 22.996L21.7538 19.7697L21.7538 26.9258H18.9391V19.7697ZM34.4204 24.8152C36.3636 24.8152 37.9389 23.2402 37.9389 21.2975C37.9389 19.3547 36.3636 17.7798 34.4204 17.7798C32.4772 17.7798 30.902 19.3547 30.902 21.2975C30.902 23.2402 32.4772 24.8152 34.4204 24.8152ZM34.4204 27.6293C30.9226 27.6293 28.0872 24.7945 28.0872 21.2975C28.0872 17.8005 30.9226 14.9656 34.4204 14.9656C37.9182 14.9656 40.7537 17.8005 40.7537 21.2975C40.7537 24.7945 37.9182 27.6293 34.4204 27.6293ZM41.0452 35.6597L39.0549 33.6698L35.8278 36.8961V29.7399H33.013V36.8961L29.786 33.6698L27.7957 35.6597L34.4204 42.283L41.0452 35.6597ZM17.5317 38.1824H23.1612L23.1612 32.5541H17.5317V38.1824ZM24.5687 29.7399C25.346 29.7399 25.9761 30.3699 25.9761 31.147V39.5895C25.9761 40.3667 25.346 40.9966 24.5687 40.9966H16.1242C15.347 40.9966 14.7168 40.3667 14.7169 39.5895V31.147C14.7169 30.3699 15.347 29.7399 16.1243 29.7399H24.5687Z"
                    fill="#011B58"
                  />
                </svg>
              </div>
              <h4 className="text-xl font-bold mb-4">
                Custom deployment options
              </h4>
              <div>
                For companies looking to build AI-driven solutions that solve
                complex supply chain problems, AppDirect has partnered with
                IVADO Labs, a leading AI solution provider with a mission to
                take advanced AI technologies out of the lab and apply them in
                the real world.
              </div>
            </div>
          </div>
        </div>
        <BlobAnimation />
      </div>

      <LandingTutorials />

      <div className="flex flex-col items-center mb-14 mt-20 bg-navy ">
        <div className="lg:w-[1000px] py-32 text-white mx-10">
          <q className="text-2xl md:text-4xl italic font-serif font-light leading-relaxed">
            At AppDirect, we&apos;re democratizing the AI space. Creating Gen AI
            apps is quick and simple, anyone can get one up and running within
            minutes, no coding skills required.
          </q>
          <h4 className="text-xl font-bold mt-16">Peush Patel</h4>
          <div>VP Product Management / AppDirect</div>
        </div>
      </div>

      <div className="flex flex-col items-center mb-14 mt-24 mx-4">
        <h3 className="text-3xl font-bold mb-8 text-center">
          Explore the AppDirect AI Marketplace
        </h3>
        <h4 className="text-xl mb-11 lg:w-[710px] text-center">
          AI apps ready to use today, purpose-built to help you solve business
          problems, gain insights, and manage workloads.
        </h4>
        <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-4 gap-8 lg:w-[1140px]">
          <Link href="https://appdirect.ai/public/ai/278b08b6-947f-4253-af47-587edcfc1840">
            <div className="bg-[#F0F0F0] px-8 py-4 shadow-lg flex items-center">
              <svg
                width="54"
                height="54"
                viewBox="0 0 54 54"
                fill="none"
                xmlns="http://www.w3.org/2000/svg"
              >
                <path
                  d="M34.8879 11.5622C30.6191 11.5622 27.1587 15.0226 27.1587 19.2913C27.1587 23.5601 30.6191 27.0205 34.8879 27.0205C39.1566 27.0205 42.617 23.5601 42.617 19.2913C42.617 15.0226 39.1566 11.5622 34.8879 11.5622ZM22.742 19.2913C22.742 12.5834 28.1798 7.14551 34.8879 7.14551C41.5959 7.14551 47.0337 12.5834 47.0337 19.2913C47.0337 21.8475 46.244 24.2192 44.8954 26.1758L50.8035 32.084L47.6805 35.207L41.7723 29.2988C39.8158 30.6475 37.444 31.4372 34.8879 31.4372C28.1798 31.4372 22.742 25.9994 22.742 19.2913ZM7.28369 9.35384H18.3254V13.7705H7.28369V9.35384ZM7.28369 24.8122H18.3254V29.2288H7.28369V24.8122ZM47.0337 40.2705V44.6872H7.28369V40.2705H47.0337Z"
                  fill="#011B58"
                />
              </svg>
              <div className="ml-4">Research Assistant</div>
            </div>
          </Link>
          <Link href="https://appdirect.ai/public/ai/1cd5ae8e-81a5-4f81-b9df-f005ab67f260">
            <div className="bg-[#F0F0F0] px-8 py-4 shadow-lg flex items-center">
              <svg
                width="39"
                height="45"
                viewBox="0 0 39 45"
                fill="none"
                xmlns="http://www.w3.org/2000/svg"
              >
                <path
                  d="M16.7124 26.3812V30.4205C10.0253 30.4205 4.60876 35.8434 4.60876 42.5385H0.574219C0.574219 33.6118 7.79628 26.3812 16.7124 26.3812ZM16.7124 24.3727C10.0253 24.3727 4.60876 18.9497 4.60876 12.2547C4.60876 5.55968 10.0253 0.136719 16.7124 0.136719C23.3995 0.136719 28.816 5.55968 28.816 12.2547C28.816 18.9497 23.3995 24.3727 16.7124 24.3727ZM16.7124 20.3333C21.1705 20.3333 24.7815 16.718 24.7815 12.2547C24.7815 7.79135 21.1705 4.17604 16.7124 4.17604C12.2543 4.17604 8.64331 7.79135 8.64331 12.2547C8.64331 16.718 12.2543 20.3333 16.7124 20.3333ZM28.816 41.5342L22.8868 44.6585L24.0236 38.0528L19.2312 33.3663L25.8514 32.4067L28.816 26.4035L31.7806 32.4067L38.4009 33.3663L33.6084 38.0528L34.7452 44.6585L28.816 41.5342Z"
                  fill="#011B58"
                />
              </svg>
              <div className="ml-4">Spanish Tutor</div>
            </div>
          </Link>
          <Link href="https://appdirect.ai/public/ai/cf94a2e5-379f-4eef-9388-25d823390ea8">
            <div className="bg-[#F0F0F0] px-8 py-4 shadow-lg flex items-center">
              <svg
                width="37"
                height="47"
                viewBox="0 0 37 47"
                fill="none"
                xmlns="http://www.w3.org/2000/svg"
              >
                <path
                  d="M13.8441 35.8594H16.1345V24.7407H20.582V35.8594H22.8725C23.1615 33.1909 24.5403 30.9894 26.7418 28.5655C26.9864 28.2986 28.5875 26.6308 28.7876 26.4085C30.6778 24.0513 31.7229 21.1382 31.7229 18.0694C31.7229 10.7089 25.7411 4.72702 18.3805 4.72702C11.0199 4.72702 5.03807 10.7089 5.03807 18.0694C5.03807 21.1382 6.08323 24.0513 7.95117 26.4085C8.15131 26.6531 9.7524 28.2986 9.99701 28.5655C12.2207 30.9671 13.5772 33.1909 13.8663 35.8594H13.8441ZM13.9108 40.3068V42.5306H22.8057V40.3068H13.9108ZM4.4599 29.1881C2.01379 26.1416 0.568359 22.2723 0.568359 18.0694C0.568359 8.24052 8.52934 0.279541 18.3583 0.279541C28.1872 0.279541 36.1482 8.24052 36.1482 18.0694C36.1482 22.2723 34.6805 26.1416 32.2344 29.1881C30.8557 30.9004 27.2532 33.6356 27.2532 36.9712V42.5306C27.2532 44.9767 25.2519 46.978 22.8057 46.978H13.9108C11.4647 46.978 9.46331 44.9767 9.46331 42.5306V36.9712C9.46331 33.6356 5.83862 30.9004 4.4599 29.1881Z"
                  fill="#011B58"
                />
              </svg>
              <div className="ml-4">Blog Post Generator</div>
            </div>
          </Link>
          <Link href="https://appdirect.ai/public/ai/28344a17-9270-4677-9ac7-40bfc0b885b7">
            <div className="bg-[#F0F0F0] px-8 py-4 shadow-lg flex items-center">
              <svg
                width="55"
                height="54"
                viewBox="0 0 55 54"
                fill="none"
                xmlns="http://www.w3.org/2000/svg"
              >
                <path
                  d="M29.5491 47.1969V51.6444H25.1016V47.1969H7.31164C6.08858 47.1969 5.08789 46.1962 5.08789 44.9732V13.8407H49.5629V44.9732C49.5629 46.1962 48.5622 47.1969 47.3391 47.1969H29.5491ZM9.53539 42.7494H45.1154V18.2882H9.53539V42.7494ZM29.5491 22.7357H40.6679V27.1832H29.5491V22.7357ZM29.5491 31.6307H40.6679V36.0782H29.5491V31.6307ZM20.6541 22.7357V29.4069H27.3254C27.3254 33.0984 24.3456 36.0782 20.6541 36.0782C16.9627 36.0782 13.9829 33.0984 13.9829 29.4069C13.9829 25.7155 16.9627 22.7357 20.6541 22.7357ZM5.08789 7.16943H49.5629V11.6169H5.08789V7.16943Z"
                  fill="#011B58"
                />
              </svg>
              <div className="ml-4">Master Summarizer</div>
            </div>
          </Link>
          <Link href="https://appdirect.ai/public/ai/14112ef6-dd35-4b7a-b6ac-b71042a5416c">
            <div className="bg-[#F0F0F0] px-8 py-4 shadow-lg flex items-center">
              <svg
                width="54"
                height="55"
                viewBox="0 0 54 55"
                fill="none"
                xmlns="http://www.w3.org/2000/svg"
              >
                <path
                  d="M36.9543 9.79834C36.9543 14.5571 33.6187 18.5376 29.1712 19.5606V32.0357H24.7237V19.5606C20.2762 18.5599 16.9406 14.5571 16.9406 9.79834H12.4932C12.4932 15.3799 15.6509 20.2054 20.2762 22.6293V36.4832V49.8256H24.7237V36.4832H29.1712V49.8256H33.6187V36.4832V22.6293C38.2441 20.2277 41.4018 15.3799 41.4018 9.79834H36.9543Z"
                  fill="#011B58"
                />
                <path
                  d="M26.948 16.4695C30.0184 16.4695 32.5074 13.9805 32.5074 10.9102C32.5074 7.83983 30.0184 5.35083 26.948 5.35083C23.8777 5.35083 21.3887 7.83983 21.3887 10.9102C21.3887 13.9805 23.8777 16.4695 26.948 16.4695Z"
                  fill="#011B58"
                />
              </svg>
              <div className="ml-4">HR Specialist</div>
            </div>
          </Link>
          <Link href="https://appdirect.ai/public/ai/32e83746-d469-4188-83e7-f027080e7f0c">
            <div className="bg-[#F0F0F0] px-8 py-4 shadow-lg flex items-center">
              <svg
                width="54"
                height="54"
                viewBox="0 0 54 54"
                fill="none"
                xmlns="http://www.w3.org/2000/svg"
              >
                <path
                  d="M14.2337 5.27368C20.1726 5.27368 25.3292 8.62269 27.918 13.5351C30.5179 9.87258 34.7945 7.48201 39.6294 7.48201H47.3586V13.0028C47.3586 20.9304 40.9321 27.357 33.0044 27.357H29.6919V29.5653H40.7336V45.0237C40.7336 47.463 38.7563 49.4403 36.3169 49.4403H18.6504C16.2111 49.4403 14.2337 47.463 14.2337 45.0237V29.5653H25.2753V25.1487H20.8587C12.3213 25.1487 5.40039 18.2277 5.40039 9.69035V5.27368H14.2337ZM36.3169 33.982H18.6504V45.0237H36.3169V33.982ZM42.9419 11.8987H39.6294C34.1413 11.8987 29.6919 16.3479 29.6919 21.8362V22.9403H33.0044C38.4928 22.9403 42.9419 18.4912 42.9419 13.0028V11.8987ZM14.2337 9.69035H9.81706C9.81706 15.7885 14.7606 20.732 20.8587 20.732H25.2753C25.2753 14.6339 20.3319 9.69035 14.2337 9.69035Z"
                  fill="#011B58"
                />
              </svg>
              <div className="ml-4">Horticulturalist</div>
            </div>
          </Link>
          <Link href="https://appdirect.ai/public/ai/8a13a77a-77fc-4e9f-8957-78f08c6c071a">
            <div className="bg-[#F0F0F0] px-8 py-4 shadow-lg flex items-center">
              <svg
                width="55"
                height="54"
                viewBox="0 0 55 54"
                fill="none"
                xmlns="http://www.w3.org/2000/svg"
              >
                <path
                  d="M18.4634 9.06344C18.4634 11.5096 16.4621 13.5109 14.0159 13.5109C11.5698 13.5109 9.56846 11.5096 9.56846 9.06344C9.56846 6.61733 11.5698 4.61597 14.0159 4.61597C16.4621 4.61597 18.4634 6.61733 18.4634 9.06344ZM11.7922 35.7483V49.0907H7.34473V22.4059C7.34473 18.7145 10.3245 15.7347 14.0159 15.7347C15.8394 15.7347 17.485 16.4685 18.708 17.6471L23.9783 22.6282L29.1151 17.4914L32.2506 20.6269L24.045 28.8325L20.6649 25.6303V49.0685H16.2174V35.7261H11.77L11.7922 35.7483ZM14.0159 20.1821C12.7929 20.1821 11.7922 21.1828 11.7922 22.4059V31.3008H16.2397V22.4059C16.2397 21.1828 15.239 20.1821 14.0159 20.1821ZM42.9245 11.2872H22.9109V6.83971H45.1483C46.3713 6.83971 47.372 7.84039 47.372 9.06344V33.5246C47.372 34.7476 46.3713 35.7483 45.1483 35.7483H37.5431L43.814 49.0907H38.8996L32.6286 35.7483H22.9109V31.3008H42.9245V11.2872Z"
                  fill="#011B58"
                />
              </svg>

              <div className="ml-4">English Tutor</div>
            </div>
          </Link>
          <Link href="https://appdirect.ai/public/ai/f8a3c7cb-e752-4a60-bd89-a2b312fa2138">
            <div className="bg-[#F0F0F0] px-8 py-4 shadow-lg flex items-center">
              <svg
                width="54"
                height="54"
                viewBox="0 0 54 54"
                fill="none"
                xmlns="http://www.w3.org/2000/svg"
              >
                <path
                  d="M29.3485 39.8487V44.2654H42.5985V48.682H29.3485C26.9193 48.682 24.9318 46.6945 24.9318 44.2654V39.8487H18.3068C13.4264 39.8487 9.47351 35.8958 9.47351 31.0154V15.557C9.47351 14.3425 10.4673 13.3487 11.6818 13.3487H18.3068V4.51538H22.7235V13.3487H31.5568V4.51538H35.9735V13.3487H42.5985C43.8131 13.3487 44.8068 14.3425 44.8068 15.557V31.0154C44.8068 35.8958 40.8539 39.8487 35.9735 39.8487H29.3485ZM18.3068 35.432H35.9735C38.4027 35.432 40.3902 33.4445 40.3902 31.0154V24.3904H13.8902V31.0154C13.8902 33.4445 15.8777 35.432 18.3068 35.432ZM40.3902 17.7654H13.8902V19.9737H40.3902V17.7654Z"
                  fill="#011B58"
                />
              </svg>
              <div className="ml-4">Expert Electrician</div>
            </div>
          </Link>
        </div>
      </div>

      <LandingCTA />

      <LandingFooter />
    </div>
  );
};

export default LandingHome;
