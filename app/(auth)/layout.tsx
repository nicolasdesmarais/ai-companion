import Image from "next/image";
import { Card, CardFooter, CardHeader } from "@/components/ui/card";

export function Avatar() {
  return;
}

const AuthLayout = ({ children }: { children: React.ReactNode }) => {
  return (
    <div className="blue-bg flex flex-col">
      <div className="flex justify-center items-center flex-col">
        <Image
          src="/AppDirect-Mark_White.png"
          alt="AppDirect Logo"
          width="64"
          height="64"
          className="mt-5"
        />
        <h1 className="mt-5 text-2xl leading-none tracking-tight md:text-5xl lg:text-4xl dark:text-white">
          AppDirect AI
        </h1>
      </div>
      <div className="flex justify-center h-full my-16">
        <div className="auth-side-panel">
          <h2 className="me-4 mb-4 font-extrabold leading-none tracking-tight text-gray-900 text-4xl dark:text-white">
            Let AI change the way you work.
          </h2>
          <div>
            AppDirect gives you the ability to access AIs that help you be more
            successful in work and life. You can browse our catalog of community
            made AIs or create your own!
          </div>
          <div className="flex flex-wrap teaser">
            <div>
              <div className="m-3 w-56">
                <Card className="bg-card rounded-xl cursor-pointer hover:opacity-75 transition border-0 p-1">
                  <div className="h-full flex flex-col justify-between">
                    <CardHeader className="flex">
                      <div className="relative w-full h-56">
                        <Image
                          src="/poet_jobs.png"
                          alt="Poet Jobs"
                          width="220"
                          height="220"
                          className="rounded-xl object-cover"
                        />
                      </div>
                      <p className="font-bold">Steve Jobs Poet</p>
                      <p className="text-xs">
                        Steve Jobs waxes poetic over 21st century technology or
                        any other subjects you wish to bring...
                      </p>
                    </CardHeader>
                    <CardFooter className="flex justify-between text-xs text-muted-foreground mt-2">
                      <p className="lowercase">@jasper</p>
                      <div className="flex items-center">23</div>
                    </CardFooter>
                  </div>
                </Card>
              </div>
              <div className="m-3 w-56">
                <Card className="bg-card rounded-xl cursor-pointer hover:opacity-75 transition border-0 p-1">
                  <div className="h-full flex flex-col justify-between">
                    <CardHeader className="flex">
                      <div className="relative w-full h-56">
                        <Image
                          src="/monalisa.png"
                          alt="Monalisa"
                          width="220"
                          height="220"
                          className="rounded-xl object-cover"
                        />
                      </div>
                      <p className="font-bold">Monalisa</p>
                      <p className="text-xs">
                        The famous painting by Leonardo da Vinci comes to life
                        and speaks to you...
                      </p>
                    </CardHeader>
                    <CardFooter className="flex justify-between text-xs text-muted-foreground mt-2">
                      <p className="lowercase">@jasper</p>
                      <div className="flex items-center">23</div>
                    </CardFooter>
                  </div>
                </Card>
              </div>
            </div>
            <div>
              <div className="mt-3 pt-14 w-56">
                <Card className="bg-card rounded-xl cursor-pointer hover:opacity-75 transition border-0 p-1">
                  <div className="h-full flex flex-col justify-between">
                    <CardHeader className="flex">
                      <div className="relative w-full h-56">
                        <Image
                          src="/queen_jazz.png"
                          alt="Queen of Jazz"
                          width="220"
                          height="220"
                          className="rounded-xl object-cover"
                        />
                      </div>
                      <p className="font-bold">Queen of Jazz</p>
                      <p className="text-xs">
                        Ella Fitzgerald takes you through a journey of jazz
                        music and history...
                      </p>
                    </CardHeader>
                    <CardFooter className="flex justify-between text-xs text-muted-foreground mt-2">
                      <p className="lowercase">@andy</p>
                      <div className="flex items-center">34</div>
                    </CardFooter>
                  </div>
                </Card>
              </div>
              <div className="mt-3 w-56">
                <Card className="bg-card rounded-xl cursor-pointer hover:opacity-75 transition border-0 p-1">
                  <div className="h-full flex flex-col justify-between">
                    <CardHeader className="flex">
                      <div className="relative w-full h-56">
                        <Image
                          src="/stark.png"
                          alt="Tony Stark"
                          width="220"
                          height="220"
                          className="rounded-xl object-cover"
                        />
                      </div>
                      <p className="font-bold">Tony Stark</p>
                      <p className="text-xs">
                        Tony Stark, the genius, billionaire, playboy,
                        philanthropist...
                      </p>
                    </CardHeader>
                    <CardFooter className="flex justify-between text-xs text-muted-foreground mt-2">
                      <p className="lowercase">@jasper</p>
                      <div className="flex items-center">23</div>
                    </CardFooter>
                  </div>
                </Card>
              </div>
            </div>
          </div>
          <div className="teaser-blur"></div>
        </div>
        <div className="auth-side-panel">{children}</div>
      </div>
    </div>
  );
};

export default AuthLayout;
