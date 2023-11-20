import { EntityNotFoundError } from "@/src/domain/errors/Errors";
import chatService from "@/src/domain/services/ChatService";
import { getAuthorizationContext } from "@/src/lib/authorizationUtils";
import { NextRequest, NextResponse } from "next/server";

export const maxDuration = 300;
/**
 * @swagger
 * /api/v1/ai/{aiId}/chats:
 *   get:
 *     summary: Get all chats for the AI
 *     description: Retrieves a list of all chat sessions associated with the given AI identifier.
 *     operationId: getAIChats
 *     parameters:
 *       - name: aiId
 *         in: path
 *         required: true
 *         description: The identifier of the AI whose chats are to be retrieved.
 *         schema:
 *           type: string
 *     responses:
 *       '200':
 *         description: A list of chat sessions associated with the AI.
 *         content:
 *           application/json:
 *             schema:
 *               $ref: '#/components/schemas/GetChatsResponse'
 *       '404':
 *         description: AI not found with the given identifier.
 *       '500':
 *         description: Internal Server Error
 *     security:
 *       - ApiKeyAuth: []
 * components:
 *   schemas:
 *     GetChatsResponse:
 *       type: object
 *       properties:
 *         data:
 *           type: array
 *           items:
 *             $ref: '#/components/schemas/Chat'
 *     Chat:
 *       type: object
 *       properties:
 *         id:
 *           type: string
 *           description: Unique identifier for the chat session.
 *         createdAt:
 *           type: string
 *           format: date-time
 *           description: The date and time when the chat session was created.
 *         updatedAt:
 *           type: string
 *           format: date-time
 *           description: The date and time when the chat session was last updated.
 *         name:
 *           type: string
 *           description: Name of the chat session.
 *         aiId:
 *           type: string
 *           description: Identifier of the AI associated with the chat session.
 *         userId:
 *           type: string
 *           description: Identifier of the user associated with the chat session.
 *         pinPosition:
 *           type: integer
 *           format: int32
 *           description: The position of the chat in a pinned list or similar.
 */
export async function GET(
  request: NextRequest,
  { params }: { params: { aiId: string } }
) {
  const authorizationContext = await getAuthorizationContext();
  if (!authorizationContext?.orgId || !authorizationContext?.userId) {
    return new NextResponse("Unauthorized", { status: 401 });
  }
  const { orgId, userId } = authorizationContext;

  const chatsResponse = await chatService.getAIChats(params.aiId, userId);
  return NextResponse.json(chatsResponse);
}

/**
 * @swagger
 * /api/v1/ai/{aiId}/chats:
 *   post:
 *     summary: Create a new chat session for the AI
 *     description: Creates a new chat session associated with the given AI identifier and returns the created chat session data.
 *     operationId: createAIChat
 *     parameters:
 *       - name: aiId
 *         in: path
 *         required: true
 *         description: The identifier of the AI with which the chat session is to be associated.
 *         schema:
 *           type: string
 *     requestBody:
 *       required: true
 *       content:
 *         application/json:
 *           schema:
 *             type: object
 *             properties:
 *               name:
 *                 type: string
 *                 description: Name of the chat session.
 *             required:
 *               - name
 *     responses:
 *       '201':
 *         description: Chat session successfully created.
 *         content:
 *           application/json:
 *             schema:
 *               $ref: '#/components/schemas/Chat'
 *       '404':
 *         description: Not found, when the specified AI ID does not exist or is not visible to the current user.
 *       '500':
 *         description: Internal Server Error
 *     security:
 *       - ApiKeyAuth: []
 * components:
 *   schemas:
 *     Chat:
 *       type: object
 *       properties:
 *         id:
 *           type: string
 *           description: Unique identifier for the chat session.
 *         createdAt:
 *           type: string
 *           format: date-time
 *           description: The date and time when the chat session was created.
 *         updatedAt:
 *           type: string
 *           format: date-time
 *           description: The date and time when the chat session was last updated.
 *         name:
 *           type: string
 *           description: Name of the chat session.
 *         aiId:
 *           type: string
 *           description: Identifier of the AI associated with the chat session.
 *         userId:
 *           type: string
 *           description: Identifier of the user who created the chat session.
 *         pinPosition:
 *           type: integer
 *           format: int32
 *           nullable: true
 *           description: The position of the chat in a pinned list or similar. Can be null.
 */
export async function POST(
  request: NextRequest,
  { params }: { params: { aiId: string } }
) {
  const authorizationContext = await getAuthorizationContext();
  if (!authorizationContext?.orgId || !authorizationContext?.userId) {
    return new NextResponse("Unauthorized", { status: 401 });
  }
  const { orgId, userId } = authorizationContext;

  try {
    const conversation = await chatService.createChat(
      orgId,
      userId,
      params.aiId
    );
    return NextResponse.json(conversation, { status: 201 });
  } catch (error) {
    if (error instanceof EntityNotFoundError) {
      return new NextResponse(error.message, { status: 404 });
    }
    return new NextResponse(error.message, { status: 500 });
  }
}
