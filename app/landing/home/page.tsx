import LandingFooter from "@/components/landing-footer";
import LandingNav from "@/components/landing-nav";
import LandingStartChat from "@/components/landing-start-chat";
import LandingTutorials from "@/components/landing-tutorials";
import { ArrowRight } from "lucide-react";
import Link from "next/link";

const LandingHome = () => {
  return (
    <div className="bg-white flex flex-col text-navy">
      <LandingNav />
      <div className="max-h-[785px] h-screen mb-16 overflow-hidden flex flex-col mx-4">
        <div className="flex flex-col md:flex-row justify-center  items-center">
          <div className="mt-24 md:mr-16 w-[440px] text-center lg:text-left">
            <h2
              className="me-4 mb-8 font-extrabold leading-none tracking-tight text-4xl md:text-5xl lg:text-6xl"
              title="AI made simple"
            >
              AppDirect AI Marketplace & Creation Studio
            </h2>
            <div className="mb-8">
              Create custom AIs for yourself, your team, or your customers in
              under 30 minutes. No coding required.
            </div>
            <div className="flex justify-evenly">
              <Link href="/sign-up" className="px-8 py-2 bg-sky">
                Sign up
              </Link>
              <Link href="/sign-up" className="px-4 py-2">
                Take tour
                <ArrowRight className="inline-block w-4 h-4 ml-2" />
              </Link>
            </div>
          </div>
          <div className="mt-20 shadow-glow">
            <video width="640" height="420" preload="none" autoPlay loop muted>
              <source src="/create-demo.mp4" type="video/mp4" />
            </video>
          </div>
        </div>
      </div>

      <LandingStartChat />

      <div className="flex flex-col items-center mb-14 mt-20">
        <div className="blue-bg lg:bg-unleash-pattern lg:w-[1110px] px-20 py-16 flex flex-col items-center mx-4">
          <h3 className="text-3xl font-bold mb-16">
            Unleash productivity and innovation
          </h3>
          <div className="grid grid-cols-1 lg:grid-cols-3 gap-8">
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
            <div className="bg-navy text-white px-8 py-16 drop-shadow-lg">
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
                For companies ready to build a fully custom, enterprise-ready AI
                solution, AppDirect partner Ivado labs provides professional
                services to help develop your vision, strategy, and
                implementation.
              </div>
            </div>
          </div>
        </div>
      </div>

      <LandingTutorials />

      <div className="flex flex-col items-center mb-14 mt-20 bg-navy">
        <div className="w-[1000px] py-32 text-white">
          <q className="text-4xl italic font-serif font-light leading-relaxed">
            At AppDirect, we&apos;re democratizing the AI space. Creating apps
            is quick and simple, anyone can get one up and running within
            minutes, no coding skills required.
          </q>
          <h4 className="text-xl font-bold mt-16">Peush Patel</h4>
          <div>VP Product Management / AppDirect</div>
        </div>
      </div>

      <div className="flex flex-col items-center mb-14 mt-24">
        <h3 className="text-3xl font-bold mb-8">
          Explore the AppDirect AI Marketplace
        </h3>
        <h4 className="text-xl mb-11 w-[710px] text-center">
          AI apps ready to use today, purpose-built to help you solve business
          problems, gain insights, and manage workloads.
        </h4>
        <div className="grid grid-cols-4 gap-8 w-[1100px]">
          <div className="bg-navylight px-8 py-4 drop-shadow-lg flex items-center">
            <svg
              width="54"
              height="54"
              viewBox="0 0 54 54"
              fill="none"
              xmlns="http://www.w3.org/2000/svg"
            >
              <path
                d="M11.3811 7.23352V42.8133H46.9609V47.2608H6.93359V7.23352H11.3811ZM45.382 14.5496L48.5175 17.6851L35.82 30.3826L29.1487 23.7114L19.6089 33.2513L16.4734 30.1158L29.171 17.4182L35.8422 24.0895L45.382 14.5496Z"
                fill="#011B58"
              />
            </svg>
            <div className="ml-4">Market Trend Analysis</div>
          </div>

          <div className="bg-navylight px-8 py-4 drop-shadow-lg flex items-center">
            <svg
              width="38"
              height="46"
              viewBox="0 0 38 46"
              fill="none"
              xmlns="http://www.w3.org/2000/svg"
            >
              <path
                d="M16.3169 26.8777V30.9171C9.6298 30.9171 4.21326 36.34 4.21326 43.035H0.178711C0.178711 34.1084 7.40077 26.8777 16.3169 26.8777ZM16.3169 24.8692C9.6298 24.8692 4.21326 19.4463 4.21326 12.7513C4.21326 6.05626 9.6298 0.633301 16.3169 0.633301C23.004 0.633301 28.4205 6.05626 28.4205 12.7513C28.4205 19.4463 23.004 24.8692 16.3169 24.8692ZM16.3169 20.8299C20.7749 20.8299 24.386 17.2146 24.386 12.7513C24.386 8.28793 20.7749 4.67262 16.3169 4.67262C11.8588 4.67262 8.2478 8.28793 8.2478 12.7513C8.2478 17.2146 11.8588 20.8299 16.3169 20.8299ZM28.4205 42.0308L22.4913 45.1551L23.6281 38.5494L18.8357 33.8629L25.4559 32.9033L28.4205 26.9001L31.3851 32.9033L38.0053 33.8629L33.2129 38.5494L34.3497 45.1551L28.4205 42.0308Z"
                fill="#011B58"
              />
            </svg>

            <div className="ml-4">Candidate Ranking</div>
          </div>

          <div className="bg-navylight px-8 py-4 drop-shadow-lg flex items-center">
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

          <div className="bg-navylight px-8 py-4 drop-shadow-lg flex items-center">
            <svg
              width="54"
              height="54"
              viewBox="0 0 54 54"
              fill="none"
              xmlns="http://www.w3.org/2000/svg"
            >
              <path
                d="M14.4355 31.9967L5.00684 39.4017V6.57935C5.00684 5.40077 5.96304 4.4668 7.11939 4.4668H38.8966C40.0752 4.4668 41.0092 5.423 41.0092 6.57935V31.9967H14.4355ZM12.9678 27.7716H36.7618V8.71414H9.23194V30.7069L12.9678 27.7716ZM19.8169 36.244H41.4984L45.2343 39.1794V17.1866H47.3468C48.5254 17.1866 49.4594 18.1428 49.4594 19.2991V47.8964L40.0307 40.4914H21.9295C20.7509 40.4914 19.8169 39.5351 19.8169 38.3788V36.2663V36.244Z"
                fill="#011B58"
              />
              <path
                d="M25.0651 22.5458H20.9512V26.504H25.0651V22.5458Z"
                fill="#011B58"
              />
              <path
                d="M22.9975 9.69252C20.2401 9.69252 17.8607 11.6494 17.327 14.3401L17.2158 14.8961L21.2408 15.6966L21.352 15.1407C21.5076 14.3624 22.197 13.7842 23.0198 13.7842C23.9537 13.7842 24.7098 14.5403 24.7098 15.4742C24.7098 16.4082 23.9537 17.1643 23.0198 17.1643C21.8857 17.1643 20.9739 18.076 20.9739 19.2101V21.7229H25.0879V20.9002C27.3116 20.0551 28.8237 17.8759 28.8237 15.4742C28.8237 12.2721 26.222 9.67029 23.042 9.67029L22.9975 9.69252Z"
                fill="#011B58"
              />
            </svg>

            <div className="ml-4">Support Ticket Ranking</div>
          </div>

          <div className="bg-navylight px-8 py-4 drop-shadow-lg flex items-center">
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
            <div className="ml-4">Benefits Admin</div>
          </div>

          <div className="bg-navylight px-8 py-4 drop-shadow-lg flex items-center">
            <svg
              width="54"
              height="54"
              viewBox="0 0 54 54"
              fill="none"
              xmlns="http://www.w3.org/2000/svg"
            >
              <path
                d="M27.2091 49.0905C14.934 49.0905 4.97168 39.1281 4.97168 26.8531C4.97168 14.5781 14.934 4.61572 27.2091 4.61572C39.4841 4.61572 49.4464 14.5781 49.4464 26.8531C49.4464 39.1281 39.4841 49.0905 27.2091 49.0905ZM27.2091 44.643C37.038 44.643 44.999 36.682 44.999 26.8531C44.999 17.0242 37.038 9.0632 27.2091 9.0632C17.3801 9.0632 9.41916 17.0242 9.41916 26.8531C9.41916 36.682 17.3801 44.643 27.2091 44.643ZM19.426 31.3006H31.6565C32.2792 31.3006 32.7684 30.8114 32.7684 30.1887C32.7684 29.5661 32.2792 29.0768 31.6565 29.0768H22.7616C19.6928 29.0768 17.2022 26.5863 17.2022 23.5175C17.2022 20.4487 19.6928 17.9582 22.7616 17.9582H24.9853V13.5107H29.4328V17.9582H34.9921V22.4056H22.7616C22.1389 22.4056 21.6497 22.8949 21.6497 23.5175C21.6497 24.1401 22.1389 24.6294 22.7616 24.6294H31.6565C34.7253 24.6294 37.2159 27.12 37.2159 30.1887C37.2159 33.2575 34.7253 35.7481 31.6565 35.7481H29.4328V40.1955H24.9853V35.7481H19.426V31.3006Z"
                fill="#011B58"
              />
            </svg>
            <div className="ml-4">Expense Management</div>
          </div>

          <div className="bg-navylight px-8 py-4 drop-shadow-lg flex items-center">
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

            <div className="ml-4">Training Sims</div>
          </div>

          <div className="bg-navylight px-8 py-4 drop-shadow-lg flex items-center">
            <svg
              width="54"
              height="54"
              viewBox="0 0 54 54"
              fill="none"
              xmlns="http://www.w3.org/2000/svg"
            >
              <path
                d="M5.6524 14.4288C6.51965 13.5615 7.92061 13.5615 8.78787 14.4288L11.6565 17.2974L14.5251 14.4288C15.3924 13.5615 16.7933 13.5615 17.6606 14.4288C18.5278 15.296 18.5278 16.697 17.6606 17.5642L14.792 20.4329L17.6606 23.3015C18.5278 24.1687 18.5278 25.5697 17.6606 26.4369C16.7933 27.3042 15.3924 27.3042 14.5251 26.4369L11.6565 23.5683L8.78787 26.4369C7.92061 27.3042 6.51965 27.3042 5.6524 26.4369C4.78514 25.5697 4.78514 24.1687 5.6524 23.3015L8.52102 20.4329L5.6524 17.5642C4.78514 16.697 4.78514 15.296 5.6524 14.4288Z"
                fill="#011B58"
              />
              <path
                d="M42.0994 21.189C37.5852 21.189 33.916 17.5198 33.916 13.0056C33.916 8.49143 37.5852 4.82227 42.0994 4.82227C46.6136 4.82227 50.2827 8.49143 50.2827 13.0056C50.2827 17.5198 46.6136 21.189 42.0994 21.189ZM42.0994 9.29198C40.0535 9.29198 38.3635 10.9598 38.3635 13.0279C38.3635 15.0959 40.0313 16.7637 42.0994 16.7637C44.1674 16.7637 45.8353 15.0959 45.8353 13.0279C45.8353 10.9598 44.1674 9.29198 42.0994 9.29198Z"
                fill="#011B58"
              />
              <path
                d="M36.7842 36.6661C37.6515 35.7988 39.0524 35.7988 39.9197 36.6661L42.7883 39.5347L45.6569 36.6661C46.5242 35.7988 47.9252 35.7988 48.7924 36.6661C49.6597 37.5333 49.6597 38.9343 48.7924 39.8015L45.9238 42.6702L48.7924 45.5388C49.6597 46.406 49.6597 47.807 48.7924 48.6743C47.9252 49.5415 46.5242 49.5415 45.6569 48.6743L42.7883 45.8056L39.9197 48.6743C39.0524 49.5415 37.6515 49.5415 36.7842 48.6743C35.917 47.807 35.917 46.406 36.7842 45.5388L39.6529 42.6702L36.7842 39.8015C35.917 38.9343 35.917 37.5333 36.7842 36.6661Z"
                fill="#011B58"
              />
              <path
                d="M25.5989 25.8809C18.016 25.8809 11.834 32.0629 11.834 39.6458V49.408H16.237V39.6458C16.237 34.4867 20.4399 30.3061 25.5989 30.3061H36.8955V25.9031H25.5989V25.8809Z"
                fill="#011B58"
              />
              <path
                d="M33.916 19.1431L30.8027 22.2785L35.2057 26.6815V29.5279L30.8027 33.9532L33.916 37.0664L42.8776 28.1047L33.916 19.1431Z"
                fill="#011B58"
              />
            </svg>
            <div className="ml-4">Customer Journey Analysis</div>
          </div>
        </div>
      </div>

      <div className="flex flex-col items-center mb-14 mt-20">
        <div className="bg-cta-pattern bg-center w-[1100px] py-28 px-32">
          <div className="flex bg-white py-12 px-20 justify-evenly items-center">
            <h3 className="text-3xl font-bold w-[400px]">
              Start chatting with or creating AI apps today.
            </h3>
            <Link
              href="/sign-up"
              className="ml-10 px-8 py-2 bg-navy text-white flex items-center"
            >
              Get Started
              <ArrowRight className="inline-block w-4 h-4 ml-2" />
            </Link>
          </div>
        </div>
      </div>

      <LandingFooter />
    </div>
  );
};

export default LandingHome;
