import { ApiError } from "@/src/domain/ports/api/ApiError";
import { NextResponse } from "next/server";
import { EntityNotFoundError, ForbiddenError } from "../domain/errors/Errors";

type Handler = (...args: any[]) => Promise<any>;

const errorStatusMap = new Map<Function, number>([
  [EntityNotFoundError, 404],
  [ForbiddenError, 403],
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
      // Check if error type is known and has a mapped status code
      const status = errorStatusMap.get(error.constructor);
      if (status) {
        return createApiErrorResponse(error.message, status);
      }

      // Handle generic errors
      return createApiErrorResponse("Internal Server Error", 500);
    }
  };
};
