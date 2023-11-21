import { ApiError } from "@/src/ports/api/ApiError";
import { NextResponse } from "next/server";
import { EntityNotFoundError, ForbiddenError } from "../domain/errors/Errors";

// This function takes a handler and returns a new handler that includes the try-catch logic
export const withErrorHandler = (handler: Function) => {
  return async (...args: any[]) => {
    try {
      // Call the original handler
      return await handler(...args);
    } catch (error) {
      let apiError: ApiError;

      // Handle known error types
      if (error instanceof EntityNotFoundError) {
        apiError = {
          message: error.message,
        };
        return new NextResponse(JSON.stringify(apiError), {
          status: 404,
        });
      }
      if (error instanceof ForbiddenError) {
        apiError = {
          message: error.message,
        };

        return new NextResponse(JSON.stringify(apiError), { status: 403 });
      }
      // Handle generic errors
      apiError = {
        message: "Internal Error",
      };
      return new NextResponse(JSON.stringify(apiError), { status: 500 });
    }
  };
};
