import { ClerkProvider } from "@clerk/nextjs";
import type { Metadata } from "next";
import { Inter } from "next/font/google";
import "./globals.css";
import Script from "next/script";

import { ThemeProvider } from "@/components/theme-provider";
import { Toaster } from "@/components/ui/toaster";
import { cn } from "@/src/lib/utils";

const inter = Inter({ subsets: ["latin"] });

export const metadata: Metadata = {
  title: "AppDirect AI",
  description: "Custom AI for your business.",
};

export default function RootLayout({
  children,
}: {
  children: React.ReactNode;
}) {
  return (
    <ClerkProvider>
      <html lang="en" suppressHydrationWarning>
        <body className={cn("bg-secondary", inter.className)}>
          <Script src="https://www.googletagmanager.com/gtag/js?id=G-B3276G1T2V" />
          <Script id="google-analytics">
            {`
              window.dataLayer = window.dataLayer || [];
              function gtag(){dataLayer.push(arguments);}
              gtag('js', new Date());
              gtag('config', 'G-B3276G1T2V');
            `}
          </Script>
          <Script id="google-tag-manager">
            {`
              (function(w,d,s,l,i){w[l]=w[l]||[];w[l].push({'gtm.start':
              new Date().getTime(),event:'gtm.js'});var f=d.getElementsByTagName(s)[0],
              j=d.createElement(s),dl=l!='dataLayer'?'&l='+l:'';j.async=true;j.src=
              'https://www.googletagmanager.com/gtm.js?id='+i+dl;f.parentNode.insertBefore(j,f);
              })(window,document,'script','dataLayer','GTM-K9NKXG6');
            `}
          </Script>
          <noscript
            dangerouslySetInnerHTML={{
              __html: `
                <iframe 
                  src=“https://www.googletagmanager.com/ns.html?id=GTM-K9NKXG6”
                  height=“0" 
                  width=“0” 
                  style=“display:none;visibility:hidden”>
                </iframe>`,
            }}
          ></noscript>
          <ThemeProvider attribute="class" defaultTheme="dark" enableSystem>
            {children}
            <Toaster />
          </ThemeProvider>
        </body>
      </html>
    </ClerkProvider>
  );
}
