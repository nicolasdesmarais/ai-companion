import { authMiddleware } from "@clerk/nextjs";
import { NextResponse } from "next/server";

// This example protects all routes including api/trpc routes
// Please edit this to allow other routes to be public as needed.
// See https://clerk.com/docs/references/nextjs/auth-middleware for more information about configuring your middleware
export default authMiddleware({
  publicRoutes: [
    "/api/webhook",
    "/api/v1/integrations/clerkWebhooks",
    "/api/v1/integrations/apify/webhooks",
    "/api/inngest",
    "/landing(.*)",
    "/login",
    "/signup",
    "/contact",
    "/api/v1/waitlist/export",
    "/api/v1/waitlist",
    "/api/v1/warm",
    "/public(.*)",
  ],
  apiRoutes: [
    "/api/((?!webhook|v1/integrations/onedrive|v1/warm|v1/integrations/clerkWebhooks|v1/integrations/apify/webhooks|inngest).*)",
  ],

  afterAuth(auth, req, evt) {
    // handle users who aren't authenticated
    if (!auth.userId && !auth.isPublicRoute && !auth.isApiRoute) {
      const landing = new URL("/landing", req.url);
      return NextResponse.redirect(landing, { status: 308 });
    }
    // redirect them to organization selection page
    if (
      auth.userId &&
      !auth.orgId &&
      !auth.isPublicRoute &&
      !auth.isApiRoute &&
      req.nextUrl.pathname !== "/org-selection"
    ) {
      const orgSelection = new URL("/org-selection", req.url);
      return NextResponse.redirect(orgSelection, { status: 308 });
    }
  },
});

export const config = {
  matcher: ["/((?!.+\\.[\\w]+$|_next).*)", "/", "/(api|trpc)(.*)"],
};
