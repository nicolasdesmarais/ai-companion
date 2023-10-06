import { currentUser } from "@clerk/nextjs";
import type { NextApiRequest, NextApiResponse } from 'next'
import { AIService } from "@/domain/services/AIService";
import { ListAIsRequestParams, ListAIsRequestScope } from "@/domain/services/dtos/ListAIsRequestParams";

export async function GET(
  request: NextApiRequest,
  res: NextApiResponse) {
  try {
    const { query, method } = request;

    const user = await currentUser();
    if (!user?.id) {
      res.status(401).json("Unauthorized");
      return;
    }

    const scope = query.scope as ListAIsRequestScope || ListAIsRequestScope.ALL;

    const aiService = new AIService();
    const ais = await aiService.findAIsForUser(user.id, { scope: scope });

    res.status(200).json(ais);
  } catch (error) {
    console.log("Error on [GET /v1/me/ai]", error);
    res.status(500).json("Internal Error");
  }
};

