import { ApiError } from "@/src/adapter-in/api/ApiError";
import { NextResponse } from "next/server";
import {
  BadRequestError,
  EntityNotFoundError,
  ForbiddenError,
  RateLimitError,
} from "../domain/errors/Errors";

type Handler = (...args: any[]) => Promise<any>;

const errorStatusMap = new Map<Function, number>([
  [BadRequestError, 400],
  [EntityNotFoundError, 404],
  [ForbiddenError, 403],
  [RateLimitError, 429],
]);

const createApiErrorResponse = (message: string, status: number) => {
  const apiError: ApiError = { message };
  return new NextResponse(JSON.stringify(apiError), {
    status,
    headers: { "Content-Type": "application/json" },
  });
};

export const withErrorHandler = (handler: Handler) => {
  return async (...args: any[]) => {
    try {
      // Call the original handler
      return await handler(...args);
    } catch (error) {
      console.log(error);
      const status = errorStatusMap.get(error.constructor);
      if (status) {
        return createApiErrorResponse(error.message, status);
      }

      // Handle generic errors
      return createApiErrorResponse("Internal Server Error", 500);
    }
  };
};
