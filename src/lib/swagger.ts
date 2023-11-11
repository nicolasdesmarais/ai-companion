import { createSwaggerSpec } from "next-swagger-doc";

export const getApiDocs = async () => {
  createSwaggerSpec();
  const spec = createSwaggerSpec({
    apiFolder: "app/api/v1",
    definition: {
      openapi: "3.0.0",
      info: {
        title: "AppDirect AI API",
        version: "1.0",
      },
      components: {
        securitySchemes: {
          BearerAuth: {
            type: "http",
            scheme: "bearer",
            bearerFormat: "JWT",
          },
        },
      },
      security: [],
    },
  });
  return spec;
};
