import { Webhook } from 'svix'
import { headers } from 'next/headers'
import { WebhookEvent } from '@clerk/nextjs/server'
import { UserService } from '../../../domain/services/UserService'
import { WorkspaceService } from '../../../domain/services/WorkspaceService'


export async function POST(req: Request) {

  // You can find this in the Clerk Dashboard -> Webhooks -> choose the webhook
  const CLERK_WEBHOOK_SECRET = process.env.CLERK_WEBHOOK_SECRET

  if (!CLERK_WEBHOOK_SECRET) {
    throw new Error('Please add CLERK_WEBHOOK_SECRET from Clerk Dashboard to .env or .env.local')
  }

  // Get the headers
  const headerPayload = headers();
  const svix_id = headerPayload.get("svix-id");
  const svix_timestamp = headerPayload.get("svix-timestamp");
  const svix_signature = headerPayload.get("svix-signature");

  // If there are no headers, error out
  if (!svix_id || !svix_timestamp || !svix_signature) {
    return new Response('Error occured -- no svix headers', {
      status: 400
    })
  }

  // Get the body
  const payload = await req.json()
  const body = JSON.stringify(payload);

  // Create a new SVIX instance with your secret.
  const wh = new Webhook(CLERK_WEBHOOK_SECRET);

  let evt: WebhookEvent

  // Verify the payload with the headers
  try {
    evt = wh.verify(body, {
      "svix-id": svix_id,
      "svix-timestamp": svix_timestamp,
      "svix-signature": svix_signature,
    }) as WebhookEvent
  } catch (err) {
    console.error('Error verifying webhook:', err);
    return new Response('Error occured', {
      status: 400
    })
  }

  // Get the ID and type
  const data = evt.data;
  const eventType = evt.type;

  console.log(`Webhook with and ID of ${data.id} and type of ${eventType}`)
  console.log('Webhook body:', body)

  const primaryEmail = getPrimaryEmail(evt.data);
  console.log('Primary email: ' + primaryEmail);
  const user = {
    externalId: data.id,
    email: primaryEmail
  };

  const userService = new UserService();
  const workspaceService = new WorkspaceService();

  const createdUserEntity = await userService.create(user);
  workspaceService.addUserToNewOrExistingWorkspace(createdUserEntity);

  return new Response('', { status: 201 })
}

const getPrimaryEmail  = (data: any): string => {
  const primaryEmailId = data.primary_email_address_id;
  const primaryEmail = data.email_addresses.find((emailAddress: any) => emailAddress.id === primaryEmailId);
  return primaryEmail ? primaryEmail.email_address : null;
}
