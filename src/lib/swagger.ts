import { createSwaggerSpec } from "next-swagger-doc";

export const getApiDocs = async () => {
  createSwaggerSpec();
  const spec = createSwaggerSpec({
    apiFolder: "app/api/",
    definition: {
      openapi: "3.0.0",
      info: {
        title: "AppDirect AI API",
        version: "1.0",
      },
      components: {
        securitySchemes: {
          ApiKeyAuth: {
            type: "apiKey",
            in: "header",
            name: "Authorization",
            description:
              "Enter your bearer token in the format **Bearer {token}**",
          },
        },
      },
      security: [],
    },
  });
  return spec;
};
