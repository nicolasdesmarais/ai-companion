export const config = {
  runtime: "edge",
};

export async function POST(req: Request) {
  console.log("CRON request received");
  console.log("req:", req);
}
