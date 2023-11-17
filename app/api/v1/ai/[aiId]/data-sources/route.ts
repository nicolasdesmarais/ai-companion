import dataSourceService from "@/src/domain/services/DataSourceService";
import { getAuthorizationContext } from "@/src/lib/authorizationUtils";
import { NextResponse } from "next/server";

/**
 * @swagger
 * /api/v1/ai/{aiId}/data-sources:
 *   get:
 *     summary: Get data sources for the AI
 *     description: Retrieves a list of data sources associated with the given AI identifier.
 *     operationId: getDataSources
 *     parameters:
 *       - name: aiId
 *         in: path
 *         required: true
 *         description: The identifier of the AI whose data sources are to be retrieved.
 *         schema:
 *           type: string
 *     responses:
 *       '200':
 *         description: A list of data sources associated with the AI.
 *         content:
 *           application/json:
 *             schema:
 *               $ref: '#/components/schemas/GetDataSourcesResponse'
 *       '404':
 *         description: AI not found with the given identifier.
 *       '500':
 *         description: Internal Server Error
 *     security:
 *       - ApiKeyAuth: []
 * components:
 *   schemas:
 *     GetDataSourcesResponse:
 *       type: object
 *       properties:
 *         data:
 *           type: array
 *           items:
 *             $ref: '#/components/schemas/DataSource'
 *     DataSource:
 *       type: object
 *       properties:
 *         id:
 *           type: string
 *           description: Unique identifier for the data source.
 *         createdAt:
 *           type: string
 *           format: date-time
 *           description: The date and time when the data source was created.
 *         updatedAt:
 *           type: string
 *           format: date-time
 *           description: The date and time when the data source was last updated.
 *         lastIndexedAt:
 *           type: string
 *           format: date-time
 *           nullable: true
 *           description: The date and time when the data source was last indexed, if applicable.
 *         name:
 *           type: string
 *           description: Name of the data source.
 *         type:
 *           $ref: '#/components/schemas/DataSourceType'
 *         indexStatus:
 *           $ref: '#/components/schemas/DataSourceIndexStatus'
 *         indexPercentage:
 *           type: string
 *           description: Percentage of the indexing process completed, if applicable.
 *     DataSourceType:
 *       type: string
 *       description: The type of the data source.
 *       enum:
 *         - WEB_URL
 *         - GOOGLE_DRIVE
 *         - FILE_UPLOAD
 *     DataSourceIndexStatus:
 *       type: string
 *       description: The indexing status of the data source.
 *       enum:
 *        - INITIALIZED
 *        - INDEXING
 *        - PARTIALLY_COMPLETED
 *        - COMPLETED
 *        - FAILED
 */
export async function GET(
  request: Request,
  { params }: { params: { aiId: string } }
) {
  try {
    const authorizationContext = await getAuthorizationContext();
    if (!authorizationContext?.orgId || !authorizationContext?.userId) {
      return new NextResponse("Unauthorized", { status: 401 });
    }
    const { orgId, userId } = authorizationContext;

    const dataSources = await dataSourceService.getDataSources(
      orgId,
      userId,
      params.aiId
    );

    return NextResponse.json(dataSources);
  } catch (error) {
    console.log("[DATASOURCE_GET]", error);
    return new NextResponse("Internal Error", { status: 500 });
  }
}
