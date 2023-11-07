import { auth } from "@clerk/nextjs";
import { NextResponse } from "next/server";
import { Configuration, OpenAIApi } from "openai";
import cloudinary from "cloudinary";
import Replicate from "replicate";

export const maxDuration = 300;

const configuration = new Configuration({
  apiKey: process.env.OPENAI_API_KEY,
});

const replicate = new Replicate({
  auth: process.env.REPLICATE_API_TOKEN!,
});

const openai = new OpenAIApi(configuration);

const cloudinaryUpload = async (imageUrl: string) => {
  const data = await cloudinary.v2.uploader.unsigned_upload(
    imageUrl,
    "bawmwjeq"
  );
  return NextResponse.json(data);
};

export async function POST(req: Request) {
  try {
    const { userId } = auth();
    const body = await req.json();
    const {
      prompt,
      model = "kandinsky-21",
      amount = 1,
      resolution = "512x512",
    } = body;

    if (!userId) {
      return new NextResponse("Unauthorized", { status: 401 });
    }

    if (!configuration.apiKey) {
      return new NextResponse("OpenAI API Key not configured.", {
        status: 500,
      });
    }

    if (!prompt) {
      return new NextResponse("Prompt is required", { status: 400 });
    }

    if (!amount) {
      return new NextResponse("Amount is required", { status: 400 });
    }

    if (!resolution) {
      return new NextResponse("Resolution is required", { status: 400 });
    }

    console.log(model, prompt);

    if (model === "dalle-2") {
      const response = await openai.createImage({
        prompt,
        n: parseInt(amount, 10),
        size: resolution,
      });

      if (response.data.data && response.data.data.length > 0) {
        const imageUrl = response.data.data[0].url as string;
        return cloudinaryUpload(imageUrl);
      }
    }

    const [width, height] = resolution
      .split("x")
      .map((x: string) => parseInt(x, 10));

    if (model === "stable-diffusion-xl") {
      const output = (await replicate.run(
        "stability-ai/sdxl:2a865c9a94c9992b6689365b75db2d678d5022505ed3f63a5f53929a31a46947",
        {
          input: {
            width,
            height,
            prompt,
            refine: "expert_ensemble_refiner",
            scheduler: "K_EULER",
            lora_scale: 0.6,
            num_outputs: parseInt(amount, 10),
            guidance_scale: 7.5,
            apply_watermark: false,
            high_noise_frac: 0.8,
            negative_prompt: "",
            prompt_strength: 0.8,
            num_inference_steps: 25,
          },
        }
      )) as string[];
      if (output.length > 0) {
        const imageUrl = output[0] as string;
        return cloudinaryUpload(imageUrl);
      }
    }

    if (model === "latent-consistency") {
      const output = (await replicate.run(
        "luosiallen/latent-consistency-model:553803fd018b3cf875a8bc774c99da9b33f36647badfd88a6eec90d61c5f62fc",
        {
          input: {
            width,
            height,
            prompt,
            num_outputs: parseInt(amount, 10),
          },
        }
      )) as string[];
      const imageUrl = output[0] as string;
      return cloudinaryUpload(imageUrl);
    }

    if (model === "kandinsky-21") {
      const output = (await replicate.run(
        "ai-forever/kandinsky-2-1:a768f3c2e174c54b576cc4f222e789e161160403d0cd0ace41eeb9a0f8c8d5f8",
        {
          input: {
            task: "text2img",
            width,
            height,
            prompt,
          },
        }
      )) as string[];
      const imageUrl = output[0] as string;
      return cloudinaryUpload(imageUrl);
    }

    return new NextResponse("AI Error", { status: 500 });
  } catch (error) {
    if (error.response?.data?.error?.message) {
      return new NextResponse(error.response.data.error.message, {
        status: 422,
      });
    }
    console.log("[IMAGE_ERROR]", error);
    return new NextResponse("Internal Error", { status: 500 });
  }
}
