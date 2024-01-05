export async function POST(req: Request) {
  const headers = req.headers;
  console.log("Headers: ", headers);
  return new Response("", { status: 200 });
}
