"use client";

import Script from "next/script";

interface Props {
  userId: string;
  orgId: string;
}

export default function PendoInit({ userId, orgId }: Props) {
  if (userId) {
    return (
      <Script
        id="pendo-init"
        strategy="afterInteractive"
        dangerouslySetInnerHTML={{
          __html: `(function(apiKey){
            (function(p,e,n,d,o){var v,w,x,y,z;o=p[d]=p[d]||{};o._q=o._q||[];
            v=['initialize','identify','updateOptions','pageLoad','track'];for(w=0,x=v.length;w<x;++w)(function(m){
                o[m]=o[m]||function(){o._q[m===v[0]?'unshift':'push']([m].concat([].slice.call(arguments,0)));};})(v[w]);
                y=e.createElement(n);y.async=!0;y.src='https://cdn.pendo.io/agent/static/'+apiKey+'/pendo.js';
                z=e.getElementsByTagName(n)[0];z.parentNode.insertBefore(y,z);})(window,document,'script','pendo');
                // This function creates visitors and accounts in Pendo
                // You will need to replace <visitor-id-goes-here> and <account-id-goes-here> with values you use in your app
                // Please use Strings, Numbers, or Bools for value types.
                pendo.initialize({
                    visitor: {
                        id:              '${userId}'
                        // email:        // Recommended if using Pendo Feedback, or NPS Email
                        // full_name:    // Recommended if using Pendo Feedback
                        // role:         // Optional
                        // You can add any additional visitor level key-values here,
                        // as long as it's not one of the above reserved names.
                    },
                    account: {
                        id:              '${orgId}'
                        // name:         // Optional
                        // is_paying:    // Recommended if using Pendo Feedback
                        // monthly_value:// Recommended if using Pendo Feedback
                        // planLevel:    // Optional
                        // planPrice:    // Optional
                        // creationDate: // Optional
                        // You can add any additional account level key-values here,
                        // as long as it's not one of the above reserved names.
                    }
                });
        })('7abd62a9-6f69-4422-6b70-97f4213671d8');`,
        }}
      />
    );
  }
  return null;
}
