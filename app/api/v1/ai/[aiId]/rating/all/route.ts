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

  let distributions = [0, 0, 0, 0, 0];
  if (reviews.length) {
    const userId = reviews.map((review) => review.userId) as string[];
    const userList = await clerkClient.users.getUserList({ userId });
    const starCounts = [0, 0, 0, 0, 0];
    reviews.forEach((review: any) => {
      const user = userList.find((user) => user.id === review.userId);
      review.user = user;
      const stars = Math.round(review.rating);
      starCounts[stars - 1]++;
    });
    distributions = starCounts.map((count) => (count / reviews.length) * 100);
  }
  return NextResponse.json({ reviews, distributions });
}

export const GET = withErrorHandler(getHandler);
