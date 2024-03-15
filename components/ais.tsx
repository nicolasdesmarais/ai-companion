import Image from "next/image";
import Link from "next/link";

import { StarRating } from "@/components/star-rating";
import { Card, CardFooter, CardHeader } from "@/components/ui/card";
import { AIDetailDto } from "@/src/domain/models/AI";
import { GroupSummaryDto } from "@/src/domain/models/Groups";
import { cn, pixelCrop } from "@/src/lib/utils";
import { AuthorizationContext } from "@/src/security/models/AuthorizationContext";
import {
  BadgeCheck,
  Building,
  EyeOff,
  LockKeyhole,
  MessageSquareText,
  Users,
} from "lucide-react";
import { Tooltip } from "./ui/tooltip";

interface AIsProps {
  data: AIDetailDto[];
  authorizationContext: AuthorizationContext;
  groups: GroupSummaryDto[];
  path?: string;
}

export const AIs = ({
  data,
  authorizationContext,
  groups,
  path = "",
}: AIsProps) => {
  if (data.length === 0) {
    return (
      <div className="pt-10 flex flex-col items-center justify-center space-y-3">
        <div className="relative w-60 h-60">
          <Image fill className="grayscale" src="/empty.png" alt="Empty" />
        </div>
        <p className="text-sm text-muted-foreground">No AIs found.</p>
      </div>
    );
  }

  return (
    <div className="grid grid-cols-2 sm:grid-cols-3 md:grid-cols-4 lg:grid-cols-5 xl:grid-cols-6 2xl:grid-cols-9 gap-3 pb-10">
      {data.map((item) => (
        <Card
          key={item.name}
          className="bg-card rounded-xl cursor-pointer border-0 p-1 group"
        >
          <Link href={`${path}/ai/${item.id}`}>
            <div className="h-full flex flex-col justify-between">
              <CardHeader className="flex">
                <div className="relative w-full h-56">
                  <Image
                    src={pixelCrop(item.src, "w_250,h_250") || item.src}
                    fill
                    className="rounded-xl object-cover group-hover:opacity-75 transition"
                    alt="Character"
                  />
                  {item.chats?.length && (
                    <div className="absolute top-2 left-2">
                      <Tooltip content="Active Chat">
                        <MessageSquareText className="w-6 h-6 bg-ring px-1 rounded-md text-white" />
                      </Tooltip>
                    </div>
                  )}
                  {(item.visibility === "PRIVATE" &&
                    !item.isShared &&
                    item.userId !== authorizationContext.userId) ||
                  (item.visibility === "ORGANIZATION" &&
                    item.orgId !== authorizationContext.orgId) ||
                  (item.visibility === "GROUP" &&
                    (item.groups?.length ||
                      item.userId !== authorizationContext.userId) &&
                    !item.groups?.some((groupId) =>
                      groups.some(
                        (group) => !group.notVisibleToMe && group.id === groupId
                      )
                    )) ? (
                    <div className="absolute top-2 left-2">
                      <Tooltip content="Not Visible to Me">
                        <EyeOff className="w-6 h-6 bg-destructive px-1 rounded-md text-white" />
                      </Tooltip>
                    </div>
                  ) : null}
                  {item.visibility === "PRIVATE" && (
                    <div className="absolute top-2 right-2">
                      <Tooltip content="Private">
                        <LockKeyhole className="w-6 h-6 bg-orange px-1 rounded-md text-white" />
                      </Tooltip>
                    </div>
                  )}
                  {item.visibility === "ORGANIZATION" && (
                    <div className="absolute top-2 right-2">
                      <Tooltip content="In My Organization">
                        <Building className="w-6 h-6 bg-green px-1 rounded-md text-white" />
                      </Tooltip>
                    </div>
                  )}
                  {item.visibility === "GROUP" && (
                    <div className="absolute top-2 right-2">
                      <Tooltip content="Shared">
                        <Users className="w-6 h-6 bg-green px-1 rounded-md text-white" />
                      </Tooltip>
                    </div>
                  )}
                </div>
                <div className="flex items-center m-2 leading-5">
                  {item.isApprovedByOrg ? (
                    <BadgeCheck className="w-4 h-4 mr-1 text-ring" />
                  ) : null}
                  <p className="font-bold">{item.name}</p>
                </div>
                <p className="text-xs mx-2">{item.description}</p>
              </CardHeader>
              <CardFooter className="flex flex-col mx-2 mb-2">
                <StarRating
                  value={Math.round(item.rating)}
                  count={item.ratingCount}
                  className="mt-2"
                />
                <div className="flex justify-between text-xs text-muted-foreground mt-2">
                  <div className={cn("truncate w-11/12 rounded-sm mr-1")}>
                    {item.visibility === "PRIVATE" &&
                    item.userId === authorizationContext.userId ? (
                      <span className="bg-orange px-1 rounded-sm text-white">
                        <Tooltip content="My Creation" side="bottom">
                          {item.userName}
                        </Tooltip>
                      </span>
                    ) : (
                      <span className="lowercase">{item.userName}</span>
                    )}
                  </div>
                  <div className="flex items-center">{item.messageCount}</div>
                  <span>&nbsp;chats</span>
                </div>
              </CardFooter>
            </div>
          </Link>
        </Card>
      ))}
    </div>
  );
};
