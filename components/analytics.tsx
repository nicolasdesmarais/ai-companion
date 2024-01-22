import Script from "next/script";

export const PublicAnalytics = () => (
  <>
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
  </>
);
