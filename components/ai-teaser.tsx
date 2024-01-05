import Image from "next/image";
import { Card, CardFooter, CardHeader } from "@/components/ui/card";
import { StarRating } from "./star-rating";
import { cn } from "@/src/lib/utils";

interface Props {
  className?: string;
}

export const AITeaser = ({ className = "" }: Props) => {
  return (
    <div className={cn("h-[600px] overflow-clip", className)}>
      <div className={cn("flex flex-wrap w-[500px]")}>
        <div>
          <div className="m-3 w-56">
            <Card className="bg-card rounded-xl cursor-pointer hover:opacity-75 transition border-0 p-1">
              <div className="h-full flex flex-col justify-between">
                <CardHeader className="flex">
                  <div className="relative w-full h-56 mb-2">
                    <Image
                      src="/help_support.png"
                      alt="Help Support"
                      width="220"
                      height="220"
                      className="rounded-xl object-cover"
                    />
                  </div>
                  <p className="font-bold">Help and Support</p>
                  <p className="text-xs">
                    Here to help you with all the settings and support it takes
                    to run a marketplace.
                  </p>
                </CardHeader>
                <CardFooter className="flex flex-col">
                  <StarRating value={5} count={3} className="mt-2" />
                  <div className="flex justify-between text-xs text-muted-foreground mt-2">
                    <p className="lowercase">@jasper</p>
                    <div className="flex items-center">2 chats</div>
                  </div>
                </CardFooter>
              </div>
            </Card>
          </div>
          <div className="m-3 w-56">
            <Card className="bg-card rounded-xl cursor-pointer hover:opacity-75 transition border-0 p-1">
              <div className="h-full flex flex-col justify-between">
                <CardHeader className="flex">
                  <div className="relative w-full h-56 mb-2">
                    <Image
                      src="/quote_support.png"
                      alt="Quote Support"
                      width="220"
                      height="220"
                      className="rounded-xl object-cover"
                    />
                  </div>
                  <p className="font-bold">Quote Support</p>
                  <p className="text-xs">
                    Quote Support can generate a quote for you right here, right
                    now and you can download it directly from chat.
                  </p>
                </CardHeader>
                <CardFooter className="flex flex-col">
                  <StarRating value={0} count={0} className="mt-2" />
                  <div className="flex justify-between text-xs text-muted-foreground mt-2">
                    <p className="lowercase">@jasper</p>
                    <div className="flex items-center">23</div>
                  </div>
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
                      src="/commission_guru.png"
                      alt="Commission Guru"
                      width="220"
                      height="220"
                      className="rounded-xl object-cover"
                    />
                  </div>
                  <p className="font-bold">Commission Guru</p>
                  <p className="text-xs">
                    Discover insights into your business based on analytics of
                    big data sets from your commissions data.
                  </p>
                </CardHeader>
                <CardFooter className="flex flex-col">
                  <StarRating value={0} count={0} className="mt-2" />
                  <div className="flex justify-between text-xs text-muted-foreground mt-2">
                    <p className="lowercase">@andy</p>
                    <div className="flex items-center">34 chats</div>
                  </div>
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
                      src="/deal_architect.png"
                      alt="Deal Architect"
                      width="220"
                      height="220"
                      className="rounded-xl object-cover"
                    />
                  </div>
                  <p className="font-bold">Deal Architect</p>
                  <p className="text-xs">
                    Here to help support you with service availability &
                    pricing. He can also help you draft proposals for your
                    customer.
                  </p>
                </CardHeader>
                <CardFooter className="flex justify-between text-xs text-muted-foreground mt-2">
                  <p className="lowercase">@jasper</p>
                  <div className="flex items-center">23 chats</div>
                </CardFooter>
              </div>
            </Card>
          </div>
        </div>
      </div>
      <div className="teaser-blur"></div>
    </div>
  );
};
