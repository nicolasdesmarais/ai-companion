// pages/authenticate.tsx
import prismadb from "@/lib/prismadb";
import { auth } from "@clerk/nextjs";
import { google } from "googleapis";

const oauth2Client = new google.auth.OAuth2(
  process.env.GOOGLE_CLIENT_ID,
  process.env.GOOGLE_CLIENT_SECRET,
  "http://localhost:3000/authenticate"
);

interface GoogleDriveCallbackProps {
  queryParams: {
    code: string;
  };
}

const GoogleDriveCallback = async (params: GoogleDriveCallbackProps) => {
  // const [message, setMessage] = useState("");
  // const [status, setStatus] = useState<number | null>(null);
  const authentication = await auth();
  const userId = authentication?.userId;
  if (!userId) {
    return;
  }

  const code = params.queryParams.code;

  try {
    const { tokens } = await oauth2Client.getToken(code);

    await prismadb.googleTokens.upsert({
      where: { userId },
      update: {
        accessToken: tokens.access_token,
        refreshToken: tokens.refresh_token,
      },
      create: {
        userId,
        accessToken: tokens.access_token!,
        refreshToken: tokens.refresh_token,
      },
    });
  } catch (error) {
    console.log(error);
  }
};

export default GoogleDriveCallback;
