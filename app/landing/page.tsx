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
          <Link href="https://appdirect.ai/public/ai/836be20b-b6ca-4f3d-9191-70be800b7bea">
            <div className="bg-[#F0F0F0] px-8 py-4 shadow-lg flex items-center">
              <svg
                width="54"
                height="55"
                viewBox="0 0 54 55"
                fill="none"
                xmlns="http://www.w3.org/2000/svg"
              >
                <path
                  d="M11.3813 7.61401V43.194H46.9613V47.6415H6.93384V7.61401H11.3813ZM45.3825 14.9302L48.518 18.0656L35.8203 30.7633L29.1491 24.092L19.6092 33.6319L16.4737 30.4964L29.1713 17.7988L35.8426 24.47L45.3825 14.9302Z"
                  fill="#011B58"
                />
              </svg>

              <div className="ml-4">Accounting & Finance</div>
            </div>
          </Link>
          <Link href="https://appdirect.ai/public/ai/5b9caaf0-be67-4541-ad1b-b9784c2fb0d9">
            <div className="bg-[#F0F0F0] px-8 py-4 shadow-lg flex items-center">
              <svg
                width="54"
                height="54"
                viewBox="0 0 54 54"
                fill="none"
                xmlns="http://www.w3.org/2000/svg"
              >
                <path
                  d="M7.97957 38.1389C7.02336 36.4711 6.28952 34.7366 5.80029 32.9576C7.95733 31.8457 9.44724 29.5997 9.44724 27.0202C9.44724 24.4406 7.97957 22.1724 5.80029 21.0828C6.80098 17.5025 8.69117 14.1669 11.3597 11.4317C13.4055 12.7437 16.074 12.9216 18.32 11.6096C20.566 10.3198 21.7668 7.89592 21.6556 5.47203C25.3693 4.51582 29.1941 4.53805 32.7966 5.47203C32.6632 7.89592 33.8863 10.2976 36.1322 11.5873C38.3782 12.8771 41.069 12.7215 43.0926 11.4094C44.3824 12.7215 45.5165 14.2114 46.4949 15.8792C47.4511 17.547 48.185 19.2815 48.6742 21.0605C46.5172 22.1724 45.0272 24.4184 45.0272 26.9979C45.0272 29.5775 46.4949 31.8457 48.6742 32.9353C47.6735 36.5156 45.7833 39.8512 43.1148 42.5864C41.069 41.2744 38.4005 41.0965 36.1322 42.4085C33.864 43.7205 32.6854 46.1222 32.7966 48.5461C29.1052 49.5023 25.2581 49.48 21.6556 48.5461C21.7891 46.1222 20.566 43.7205 18.32 42.4308C16.074 41.141 13.3833 41.2966 11.3597 42.6087C10.0699 41.2966 8.93578 39.8067 7.95733 38.1389H7.97957ZM20.566 38.5837C22.9899 39.9846 24.7466 42.2084 25.5694 44.7435C26.6813 44.8546 27.7932 44.8546 28.9051 44.7435C29.7501 42.2084 31.4846 39.9846 33.9085 38.5837C36.3324 37.1827 39.1343 36.7824 41.7583 37.3384C42.4032 36.4266 42.9592 35.4704 43.4261 34.4475C41.6471 32.4461 40.602 29.8443 40.602 27.0424C40.602 24.2405 41.6471 21.6165 43.4261 19.6373C43.2038 19.1259 42.9369 18.6366 42.6701 18.1474C42.381 17.6582 42.0919 17.1912 41.7806 16.7464C39.1565 17.3024 36.3769 16.9021 33.953 15.5011C31.5291 14.1002 29.7723 11.8764 28.9495 9.34135C27.8377 9.23017 26.7258 9.23017 25.6139 9.34135C24.7689 11.8764 23.0344 14.1002 20.6105 15.5011C18.1866 16.9021 15.3847 17.3024 12.7606 16.7464C12.1157 17.6582 11.5598 18.6144 11.0928 19.6373C12.8718 21.6387 13.917 24.2405 13.917 27.0424C13.917 29.8443 12.8718 32.4684 11.0928 34.4475C11.3152 34.959 11.582 35.4482 11.8489 35.9374C12.138 36.4266 12.4271 36.8936 12.7384 37.3384C15.3624 36.7824 18.1421 37.1827 20.566 38.5837ZM27.2372 33.6914C23.5458 33.6914 20.566 30.7116 20.566 27.0202C20.566 23.3287 23.5458 20.3489 27.2372 20.3489C30.9287 20.3489 33.9085 23.3287 33.9085 27.0202C33.9085 30.7116 30.9287 33.6914 27.2372 33.6914ZM27.2372 29.2439C28.4603 29.2439 29.461 28.2432 29.461 27.0202C29.461 25.7971 28.4603 24.7964 27.2372 24.7964C26.0142 24.7964 25.0135 25.7971 25.0135 27.0202C25.0135 28.2432 26.0142 29.2439 27.2372 29.2439Z"
                  fill="#011B58"
                />
              </svg>

              <div className="ml-4">Engineering</div>
            </div>
          </Link>
          <Link href="https://appdirect.ai/public/ai/14112ef6-dd35-4b7a-b6ac-b71042a5416c">
            <div className="bg-[#F0F0F0] px-8 py-4 shadow-lg flex items-center">
              <svg
                width="55"
                height="55"
                viewBox="0 0 55 55"
                fill="none"
                xmlns="http://www.w3.org/2000/svg"
              >
                <path
                  d="M37.6562 9.70825C37.6562 14.4671 34.3206 18.4476 29.8731 19.4705V31.9458H25.4256V19.4705C20.9781 18.4698 17.6424 14.4671 17.6424 9.70825H13.1949C13.1949 15.2899 16.3527 20.1154 20.9781 22.5393V36.3933V49.7358H25.4256V36.3933H29.8731V49.7358H34.3206V36.3933V22.5393C38.946 20.1376 42.1037 15.2899 42.1037 9.70825H37.6562Z"
                  fill="#011B58"
                />
                <path
                  d="M27.6493 16.3795C30.7197 16.3795 33.2087 13.8905 33.2087 10.8201C33.2087 7.74976 30.7197 5.26074 27.6493 5.26074C24.579 5.26074 22.09 7.74976 22.09 10.8201C22.09 13.8905 24.579 16.3795 27.6493 16.3795Z"
                  fill="#011B58"
                />
              </svg>

              <div className="ml-4">Human Resources</div>
            </div>
          </Link>
          <Link href="https://appdirect.ai/public/ai/d3f4d2d4-4dc3-4964-9d93-9d0a9703cec7">
            <div className="bg-[#F0F0F0] px-8 py-4 shadow-lg flex items-center">
              <svg
                width="55"
                height="54"
                viewBox="0 0 55 54"
                fill="none"
                xmlns="http://www.w3.org/2000/svg"
              >
                <path
                  d="M9.69115 11.6147V36.076H45.2711V11.6147H9.69115ZM5.24365 9.41322C5.24365 8.16792 6.26657 7.16724 7.44516 7.16724H47.5171C48.7402 7.16724 49.7186 8.16792 49.7186 9.41322V40.5235H5.24365V9.41322ZM3.0199 42.7472H51.9424V47.1947H3.0199V42.7472Z"
                  fill="#011B58"
                />
              </svg>

              <div className="ml-4">Information Technology</div>
            </div>
          </Link>
          <Link href="https://appdirect.ai/public/ai/278b08b6-947f-4253-af47-587edcfc1840">
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
              <div className="ml-4">Learning & Development</div>
            </div>
          </Link>
          <Link href="https://appdirect.ai/public/ai/726f6e8c-a534-4f89-9066-40ef4f5b16c6">
            <div className="bg-[#F0F0F0] px-8 py-4 shadow-lg flex items-center">
              <svg
                width="54"
                height="54"
                viewBox="0 0 54 54"
                fill="none"
                xmlns="http://www.w3.org/2000/svg"
              >
                <path
                  d="M32.0183 23.589V15.2499C32.0183 14.1603 31.1288 13.2708 30.0392 13.2708H28.3713L28.2157 13.4264C24.0795 17.5626 14.2283 19.008 14.2061 19.0303H8.44655C6.55636 19.0303 5.02197 20.5647 5.02197 22.4548V31.083C5.02197 32.9732 6.55636 34.5076 8.44655 34.5076H9.425L10.8704 41.7125H14.7398V34.6187C16.9857 35.019 24.6577 36.5979 28.1934 40.1114L28.3491 40.2671H30.0169C31.1066 40.2671 31.9961 39.3776 31.9961 38.2879V29.9489C33.2858 29.4374 34.1531 28.1921 34.1531 26.7689C34.1531 25.3457 33.2858 24.1004 31.9961 23.589H32.0183ZM28.0378 18.5188V34.9968C26.0809 33.796 23.6348 32.773 20.7439 31.928C19.7432 31.6389 18.698 31.3721 17.6306 31.1275V22.3881C18.698 22.1435 19.7432 21.8767 20.7439 21.5876C23.6348 20.7426 26.0809 19.6974 28.0378 18.5188ZM13.6501 22.9885V30.5271H8.98025V22.9885H13.6501Z"
                  fill="#011B58"
                />
                <path
                  d="M43.4483 12.2256C51.476 20.2533 51.476 33.2845 43.4483 41.3345L40.3795 38.2657C46.7172 31.928 46.7172 21.632 40.3795 15.2944L43.4483 12.2256ZM37.3107 18.3631C41.9584 23.0108 41.9584 30.5715 37.3107 35.2192L34.2419 32.1504C37.1995 29.1928 37.1995 24.3895 34.2419 21.4319L37.3107 18.3631Z"
                  fill="#011B58"
                />
              </svg>
              <div className="ml-4">Marketing</div>
            </div>
          </Link>
          <Link href="https://appdirect.ai/public/ai/cf94a2e5-379f-4eef-9388-25d823390ea8">
            <div className="bg-[#F0F0F0] px-8 py-4 shadow-lg flex items-center">
              <svg
                width="55"
                height="54"
                viewBox="0 0 55 54"
                fill="none"
                xmlns="http://www.w3.org/2000/svg"
              >
                <path
                  d="M27.6379 49.1071V44.1926C33.0638 44.1926 38.4898 41.6353 41.9588 36.5429C45.8059 30.8723 45.8281 23.2004 42.0033 17.5298C35.7323 8.23452 22.9902 7.34502 15.4962 14.8613C9.29193 21.0656 8.82495 30.8278 14.073 37.5658C14.8291 38.5442 14.7401 39.923 13.8729 40.7902C12.8277 41.8354 11.0932 41.7464 10.1814 40.5679C3.31005 31.7396 4.06612 18.8863 12.4719 10.9475C20.8777 3.00871 34.4425 3.00871 42.8261 10.9475C51.899 19.5312 52.0546 33.8744 43.2708 42.6359C38.9568 46.95 33.3084 49.1071 27.6379 49.1071Z"
                  fill="#011B58"
                />
                <path
                  d="M27.6379 49.107H25.4142V20.9543H29.8617V47.0167L27.6379 49.107Z"
                  fill="#011B58"
                />
                <path
                  d="M18.6984 23.8897L21.8116 27.0029L26.2369 22.5777H29.061L33.4863 27.0029L36.5996 23.8897L27.6378 14.928L18.6984 23.8897Z"
                  fill="#011B58"
                />
              </svg>

              <div className="ml-4">Productivity</div>
            </div>
          </Link>
          <Link href="https://appdirect.ai/public/ai/7f87d6a6-e9b9-4580-82df-5ff24dbf8151">
            <div className="bg-[#F0F0F0] px-8 py-4 shadow-lg flex items-center">
              <svg
                width="55"
                height="54"
                viewBox="0 0 55 54"
                fill="none"
                xmlns="http://www.w3.org/2000/svg"
              >
                <path
                  d="M27.4809 49.3363C15.2058 49.3363 5.24341 39.3739 5.24341 27.0988C5.24341 14.8237 15.2058 4.86133 27.4809 4.86133C39.756 4.86133 49.7184 14.8237 49.7184 27.0988C49.7184 39.3739 39.756 49.3363 27.4809 49.3363ZM27.4809 44.8888C37.3099 44.8888 45.2709 36.9278 45.2709 27.0988C45.2709 17.2699 37.3099 9.30883 27.4809 9.30883C17.6519 9.30883 9.69091 17.2699 9.69091 27.0988C9.69091 36.9278 17.6519 44.8888 27.4809 44.8888ZM19.6978 31.5463H31.9284C32.5511 31.5463 33.0403 31.0571 33.0403 30.4345C33.0403 29.8118 32.5511 29.3226 31.9284 29.3226H23.0334C19.9646 29.3226 17.474 26.832 17.474 23.7632C17.474 20.6944 19.9646 18.2038 23.0334 18.2038H25.2572V13.7563H29.7047V18.2038H35.264V22.6513H23.0334C22.4108 22.6513 21.9215 23.1406 21.9215 23.7632C21.9215 24.3859 22.4108 24.8751 23.0334 24.8751H31.9284C34.9972 24.8751 37.4878 27.3657 37.4878 30.4345C37.4878 33.5032 34.9972 35.9938 31.9284 35.9938H29.7047V40.4413H25.2572V35.9938H19.6978V31.5463Z"
                  fill="#011B58"
                />
              </svg>

              <div className="ml-4">Sales</div>
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
