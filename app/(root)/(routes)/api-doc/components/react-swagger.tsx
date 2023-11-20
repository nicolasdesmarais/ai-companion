"use client";

import SwaggerUI from "swagger-ui-react";
import "swagger-ui-react/swagger-ui.css";

function ReactSwagger() {
  return <SwaggerUI url="openapi-spec.json" />;
}

export default ReactSwagger;
