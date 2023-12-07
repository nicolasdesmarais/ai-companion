import aiService from "@/src/domain/services/AIService";
import { withErrorHandler } from "@/src/middleware/ErrorMiddleware";
import { NextResponse } from "next/server";
import { clerkClient } from "@clerk/nextjs";

async function getHandler(
  request: Request,
  context: { params: { aiId: string } }
) {
  const { params } = context;

  const reviews = await aiService.getAllReviews(params.aiId);

  if (reviews.length) {
    const userId = reviews.map((review) => review.userId) as string[];
    const userList = await clerkClient.users.getUserList({ userId });
    reviews.forEach((review: any) => {
      const user = userList.find((user) => user.id === review.userId);
      review.user = user;
    });
  }

  return NextResponse.json(reviews);
}

export const GET = withErrorHandler(getHandler);
