import Image from "next/image";
import Link from "next/link";
import { Companion } from "@prisma/client";
import { MessagesSquare } from "lucide-react";

import { Card, CardFooter, CardHeader } from "@/components/ui/card";

interface CompanionsProps {
  data: (Companion & {
    _count: {
      messages: number;
    };
  })[];
}

export const Companions = ({ data }: CompanionsProps) => {
  if (data.length === 0) {
    return (
      <div className="pt-10 flex flex-col items-center justify-center space-y-3">
        <div className="relative w-60 h-60">
          <Image fill className="grayscale" src="/empty.png" alt="Empty" />
        </div>
        <p className="text-sm text-muted-foreground">No companions found.</p>
      </div>
    );
  }

  return (
    <div className="grid grid-cols-2 sm:grid-cols-3 md:grid-cols-4 lg:grid-cols-5 xl:grid-cols-6 2xl:grid-cols-9 gap-3 pb-10">
      {data.map((item) => (
        <Card
          key={item.name}
          className="bg-card rounded-xl cursor-pointer hover:opacity-75 transition border-0 p-1"
        >
          <Link href={`/ai/${item.id}`}>
            <div className="h-full flex flex-col justify-between">
              <CardHeader className="flex">
                <div className="relative w-full h-56">
                  <Image
                    src={item.src}
                    fill
                    className="rounded-xl object-cover"
                    alt="Character"
                  />
                </div>
                <p className="font-bold">{item.name}</p>
                <p className="text-xs">{item.description}</p>
              </CardHeader>
              <CardFooter className="flex justify-between text-xs text-muted-foreground mt-2">
                <p className="lowercase">@{item.userName}</p>
                <div className="flex items-center">{item._count.messages}</div>
              </CardFooter>
            </div>
          </Link>
        </Card>
      ))}
    </div>
  );
};
