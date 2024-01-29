"use client";

import SwaggerUI from "swagger-ui-react";
import "swagger-ui-react/swagger-ui.css";
import "./custom-swagger.css";
import React from "react";
import { Separator } from "@/components/ui/separator";
import { Loader } from "lucide-react";

type Props = {
  getComponent: (name: string, isIndex?: boolean) => any;
  specSelectors: any;
};
class Layout extends React.Component<Props> {
  render() {
    const { getComponent, specSelectors } = this.props;

    // list of components: https://github.com/swagger-api/swagger-ui/blob/b58589d9a41ab9df7941636404193e5515c72a4c/src/core/components/layouts/base.jsx
    const Operations = getComponent("operations", true);
    const Models = getComponent("Models", true);
    const Row = getComponent("Row");
    const Col = getComponent("Col");
    const loadingStatus = specSelectors.loadingStatus();
    if (loadingStatus === "loading") {
      return (
        <div className="flex justify-center items-center h-32">
          <Loader className="w-16 h-16 spinner" />
        </div>
      );
    }
    return (
      <div>
        <div className="pb-8">
          <div className="space-y-2 w-full mb-8 ml-4 pr-8">
            <div className="pt-8">
              <h3 className="text-lg font-medium">API Endpoints</h3>
              <p className="text-sm text-muted-foreground">
                The following API endpoints are available for your use. You can
                try them out on this page by filling out the required parameters
                and clicking Execute. Remember to include your API Bearer key in
                the X-Authorization header when making the calls from your app.
              </p>
              <p className="text-xs my-2 whitespace-pre font-mono p-4 bg-primary/10 rounded-md">
                {
                  "curl https://appdirect.ai/api/v1/me/ai -H 'X-Authorization: Bearer <your-secret-key>'"
                }
              </p>
            </div>
            <Separator className="bg-primary/10" />
          </div>
          <div className="swagger-ui">
            <Row>
              <Col mobile={12} desktop={12}>
                <Operations className="text-sm" />
              </Col>
            </Row>
          </div>
          <div className="space-y-2 w-full mb-8 ml-4 pr-8">
            <div className="pt-8">
              <h3 className="text-lg font-medium">Schema Definitions</h3>
              <p className="text-sm text-muted-foreground">
                The following data structures are used in the APIs.
              </p>
            </div>
            <Separator className="bg-primary/10" />
          </div>
          <div className="swagger-ui">
            <Row>
              <Col mobile={12} desktop={12}>
                <Models />
              </Col>
            </Row>
          </div>
        </div>
      </div>
    );
  }
}

const LayoutPlugin = () => {
  return {
    components: {
      CustomLayout: Layout,
    },
  };
};

function ReactSwagger() {
  return (
    <SwaggerUI
      url="openapi-spec.json"
      tryItOutEnabled={true}
      plugins={[LayoutPlugin]}
      layout="CustomLayout"
    />
  );
}

export default ReactSwagger;
