import { auth } from "@clerk/nextjs";
import cloudinary from "cloudinary";
import { NextResponse } from "next/server";
import OpenAI from "openai";
import Replicate from "replicate";

export const maxDuration = 300;

const replicate = new Replicate({
  auth: process.env.REPLICATE_API_TOKEN!,
});

const openai = new OpenAI({
  apiKey: process.env.OPENAI_API_KEY,
});

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

    if (!process.env.OPENAI_API_KEY) {
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

    if (model === "dalle-2") {
      const response = await openai.images.generate({
        prompt,
        n: parseInt(amount, 10),
        size: resolution,
      });

      if (response.data && response.data.length > 0) {
        const imageUrl = response.data[0].url as string;
        return cloudinaryUpload(imageUrl);
      }
    }

    if (model === "dalle-3") {
      const response = await openai.images.generate({
        prompt,
        n: parseInt(amount, 10),
        size: "1024x1024",
        model: "dall-e-3",
      });

      if (response.data && response.data.length > 0) {
        const imageUrl = response.data[0].url as string;
        return cloudinaryUpload(imageUrl);
      }
    }

    const [width, height] = resolution
      .split("x")
      .map((x: string) => parseInt(x, 10));

    if (model === "stable-diffusion-xl") {
      const output = (await replicate.run(
        "stability-ai/sdxl:39ed52f2a78e934b3ba6e2a89f5b1c712de7dfea535525255b1aa35c5565e08b",
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
        "fofr/latent-consistency-model:683d19dc312f7a9f0428b04429a9ccefd28dbf7785fef083ad5cf991b65f406f",
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

    if (model === "playground-2") {
      const output = (await replicate.run(
        "playgroundai/playground-v2-1024px-aesthetic:42fe626e41cc811eaf02c94b892774839268ce1994ea778eba97103fe1ef51b8",
        {
          input: {
            width,
            height,
            prompt,
            scheduler: "K_EULER_ANCESTRAL",
            guidance_scale: 3,
            apply_watermark: false,
            negative_prompt: "",
            num_inference_steps: 50,
          },
        }
      )) as string[];
      const imageUrl = output[0] as string;
      return cloudinaryUpload(imageUrl);
    }

    if (model === "proteus") {
      const output = (await replicate.run(
        "asiryan/proteus-v0.2:1d4e110666852b7151ad8221b8ffd7671db2c734bbd678f5331cdc99b7a6f5ae",
        {
          input: {
            width,
            height,
            prompt,
            strength: 0.8,
            scheduler: "K_EULER_ANCESTRAL",
            lora_scale: 0.6,
            num_outputs: 1,
            guidance_scale: 7,
            negative_prompt:
              "bad quality, bad anatomy, worst quality, low quality, low resolutions, extra fingers, blur, blurry, ugly, wrongs proportions, watermark, image artifacts, lowres, ugly, jpeg artifacts, deformed, noisy image",
            num_inference_steps: 40,
          },
        }
      )) as string[];
      const imageUrl = output[0] as string;
      return cloudinaryUpload(imageUrl);
    }

    if (model === "realvisxl") {
      const output = (await replicate.run(
        "adirik/realvisxl-v3.0-turbo:3dc73c805b11b4b01a60555e532fd3ab3f0e60d26f6584d9b8ba7e1b95858243",
        {
          input: {
            width,
            height,
            prompt,
            refine: "no_refiner",
            scheduler: "DPM++_SDE_Karras",
            num_outputs: 1,
            guidance_scale: 2,
            apply_watermark: false,
            high_noise_frac: 0.8,
            negative_prompt:
              "(worst quality, low quality, illustration, 3d, 2d, painting, cartoons, sketch), open mouth",
            prompt_strength: 0.8,
            num_inference_steps: 25,
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
