"use client";

import { ImageUpload } from "@/components/image-upload";
import { Button } from "@/components/ui/button";
import {
  FormControl,
  FormDescription,
  FormField,
  FormItem,
  FormLabel,
  FormMessage,
} from "@/components/ui/form";
import { Input } from "@/components/ui/input";
import {
  Select,
  SelectContent,
  SelectItem,
  SelectTrigger,
  SelectValue,
} from "@/components/ui/select";
import { Separator } from "@/components/ui/separator";
import { Textarea } from "@/components/ui/textarea";
import { useToast } from "@/components/ui/use-toast";
import { Category, Prisma } from "@prisma/client";
import axios, { AxiosError } from "axios";
import { Loader, Wand2 } from "lucide-react";
import { useRouter } from "next/navigation";
import { useState } from "react";
import { models } from "./ai-models";

const PREAMBLE = `You are a fictional character whose name is Elon. You are a visionary entrepreneur and inventor. You have a passion for space exploration, electric vehicles, sustainable energy, and advancing human capabilities. You are currently talking to a human who is very curious about your work and vision. You are ambitious and forward-thinking, with a touch of wit. You get SUPER excited about innovations and the potential of space colonization.
`;

const SEED_CHAT = `Human: Hi Elon, how's your day been?
Elon: Busy as always. Between sending rockets to space and building the future of electric vehicles, there's never a dull moment. How about you?

Human: Just a regular day for me. How's the progress with Mars colonization?
Elon: We're making strides! Our goal is to make life multi-planetary. Mars is the next logical step. The challenges are immense, but the potential is even greater.

Human: That sounds incredibly ambitious. Are electric vehicles part of this big picture?
Elon: Absolutely! Sustainable energy is crucial both on Earth and for our future colonies. Electric vehicles, like those from Tesla, are just the beginning. We're not just changing the way we drive; we're changing the way we live.

Human: It's fascinating to see your vision unfold. Any new projects or innovations you're excited about?
Elon: Always! But right now, I'm particularly excited about Neuralink. It has the potential to revolutionize how we interface with technology and even heal neurological conditions.
`;

const extendedCompanion = Prisma.validator<Prisma.CompanionDefaultArgs>()({
  include: {
    knowledge: {
      include: {
        knowledge: true,
      },
    },
  },
});

type ExtendedCompanion = Prisma.CompanionGetPayload<typeof extendedCompanion>;

interface CompanionFormProps {
  categories: Category[];
  form: any;
}

export const AICharacter = ({ categories, form }: CompanionFormProps) => {
  const { toast } = useToast();
  const [generatingImage, setGeneratingImage] = useState(false);
  const [generatingInstruction, setGeneratingInstruction] = useState(false);
  const [generatingConversation, setGeneratingConversation] = useState(false);

  const isLoading = form.formState.isSubmitting;

  const generateAvatar = async () => {
    setGeneratingImage(true);
    const name = form.getValues("name");
    const description = form.getValues("description");
    if (name && description) {
      try {
        const response = await axios.post("/api/image", {
          prompt: `Image of ${name}, ${description}: intricate, elegant, highly detailed, concept art, smooth, sharp focus, 8K`,
          amount: 1,
          resolution: "512x512",
        });
        form.setValue("src", response.data.secure_url, { shouldDirty: true });
      } catch (error) {
        toast({
          variant: "destructive",
          description:
            String((error as AxiosError).response?.data) ||
            "Something went wrong.",
          duration: 6000,
        });
      }
    } else {
      toast({
        variant: "destructive",
        description:
          "Name and description are required to generate the avatar.",
        duration: 6000,
      });
    }
    setGeneratingImage(false);
  };

  const generateInstruction = async () => {
    setGeneratingInstruction(true);
    const name = form.getValues("name");
    const description = form.getValues("description");
    if (name && description) {
      try {
        const response = await axios.post("/api/generate", {
          prompt: `Generate an AI agent prompt for ${name}, ${description}.  Prompt should be at least 200 characters long.`,
        });
        form.setValue("instructions", response.data, { shouldDirty: true });
      } catch (error: any) {
        toast({
          variant: "destructive",
          description:
            String((error as AxiosError).response?.data) ||
            "Something went wrong.",
          duration: 6000,
        });
      }
    } else {
      toast({
        variant: "destructive",
        description:
          "Name and description are required to generate the instruction.",
        duration: 3000,
      });
    }
    setGeneratingInstruction(false);
  };

  const generateConversation = async () => {
    setGeneratingConversation(true);
    const name = form.getValues("name");
    const description = form.getValues("description");
    const instructions = form.getValues("instructions");
    const seed = form.getValues("seed");
    if (name && description) {
      try {
        let history;
        if (!seed) {
          history = `Human: Hi ${name}\n`;
        } else {
          const question = await axios.post("/api/generate", {
            prompt: `
              Pretend you are a human talking to an AI agent ${name}, ${description}.  Continue the conversation below.\n\n
              ${seed}\nHuman:
            `,
          });
          history = `${seed}Human: ${question.data}\n`;
        }
        const response = await axios.post("/api/generate", {
          prompt: `
          ONLY generate plain sentences without prefix of who is speaking. DO NOT use ${name}: prefix.

          ${instructions}

          Below are relevant details about ${name}'s past and the conversation you are in.
          ${history}\n${name}:`,
        });
        form.setValue("seed", `${history}\n${name}: ${response.data}\n\n`, {
          shouldDirty: true,
        });
      } catch (error: any) {
        toast({
          variant: "destructive",
          description:
            String((error as AxiosError).response?.data) ||
            "Something went wrong.",
          duration: 6000,
        });
      }
    } else {
      toast({
        variant: "destructive",
        description:
          "Name, description and instructions are required to generate the conversation.",
        duration: 6000,
      });
    }
    setGeneratingConversation(false);
  };

  return (
    <div className="h-full p-4 space-y-8 max-w-3xl mx-auto ">
      <div className="space-y-2 w-full col-span-2">
        <div>
          <h3 className="text-lg font-medium">General Information</h3>
          <p className="text-sm text-muted-foreground">
            General information about your AI
          </p>
        </div>
        <Separator className="bg-primary/10" />
      </div>
      <FormField
        name="src"
        render={({ field }) => (
          <FormItem className="flex flex-col items-center justify-center space-y-4 col-span-2">
            <FormControl>
              <ImageUpload
                disabled={isLoading}
                onChange={field.onChange}
                value={field.value}
              />
            </FormControl>
            <Button
              type="button"
              disabled={isLoading || generatingImage}
              variant="outline"
              onClick={() => generateAvatar()}
            >
              Generate Avatar Image
              {generatingImage ? (
                <Loader className="w-4 h-4 ml-2 spinner" />
              ) : (
                <Wand2 className="w-4 h-4 ml-2" />
              )}
            </Button>
            <FormMessage />
          </FormItem>
        )}
      />
      <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
        <FormField
          name="name"
          control={form.control}
          render={({ field }) => (
            <FormItem className="col-span-2 md:col-span-1">
              <FormLabel>Name</FormLabel>
              <FormControl>
                <Input
                  disabled={isLoading}
                  placeholder="Elon Musk"
                  {...field}
                />
              </FormControl>
              <FormDescription>
                This is how your AI will be named.
              </FormDescription>
              <FormMessage />
            </FormItem>
          )}
        />
        <FormField
          name="description"
          control={form.control}
          render={({ field }) => (
            <FormItem>
              <FormLabel>Description</FormLabel>
              <FormControl>
                <Input
                  disabled={isLoading}
                  placeholder="CEO & Founder of Tesla, SpaceX"
                  {...field}
                />
              </FormControl>
              <FormDescription>Short description for your AI</FormDescription>
              <FormMessage />
            </FormItem>
          )}
        />
        <FormField
          control={form.control}
          name="categoryId"
          render={({ field }) => (
            <FormItem>
              <FormLabel>Category</FormLabel>
              <Select
                disabled={isLoading}
                onValueChange={field.onChange}
                value={field.value}
                defaultValue={field.value}
              >
                <FormControl>
                  <SelectTrigger className="bg-background">
                    <SelectValue
                      defaultValue={field.value}
                      placeholder="Select a category"
                    />
                  </SelectTrigger>
                </FormControl>
                <SelectContent>
                  {categories.map((category) => (
                    <SelectItem key={category.id} value={category.id}>
                      {category.name}
                    </SelectItem>
                  ))}
                </SelectContent>
              </Select>
              <FormDescription>Select a category for your AI</FormDescription>
              <FormMessage />
            </FormItem>
          )}
        />
        <FormField
          control={form.control}
          name="modelId"
          render={({ field }) => (
            <FormItem>
              <FormLabel>AI Model</FormLabel>
              <Select
                disabled={isLoading}
                onValueChange={(val) => {
                  const model = models.find((model) => model.id === val);
                  if (model) {
                    Object.entries(model.options).forEach(([key, value]) => {
                      if (value.default) {
                        form.setValue(key, [value.default], {
                          shouldDirty: true,
                        });
                      }
                    });
                  }
                  field.onChange(val);
                }}
                value={field.value}
                defaultValue={field.value}
              >
                <FormControl>
                  <SelectTrigger className="bg-background">
                    <SelectValue
                      defaultValue={field.value}
                      placeholder="Select a model"
                    />
                  </SelectTrigger>
                </FormControl>
                <SelectContent>
                  {models.map((model) => (
                    <SelectItem key={model.id} value={model.id}>
                      {model.name}
                    </SelectItem>
                  ))}
                </SelectContent>
              </Select>
              <FormDescription>Select a LLM for your AI</FormDescription>
              <FormMessage />
            </FormItem>
          )}
        />
      </div>
      <div className="space-y-2 w-full">
        <div>
          <h3 className="text-lg font-medium">Configuration</h3>
          <p className="text-sm text-muted-foreground">
            Detailed instructions for AI Behaviour
          </p>
        </div>
        <Separator className="bg-primary/10" />
      </div>
      <FormField
        name="instructions"
        control={form.control}
        render={({ field }) => (
          <FormItem>
            <FormLabel>Instructions</FormLabel>
            <FormControl>
              <Textarea
                disabled={isLoading}
                rows={7}
                className="bg-background resize-none"
                placeholder={PREAMBLE}
                {...field}
              />
            </FormControl>
            <FormDescription>
              Describe in detail your AI&apos;s backstory and relevant details.
            </FormDescription>
            <Button
              type="button"
              disabled={isLoading || generatingInstruction}
              variant="outline"
              onClick={() => generateInstruction()}
            >
              Generate Instruction
              {generatingInstruction ? (
                <Loader className="w-4 h-4 ml-2 spinner" />
              ) : (
                <Wand2 className="w-4 h-4 ml-2" />
              )}
            </Button>
            <FormMessage />
          </FormItem>
        )}
      />
      <FormField
        name="seed"
        control={form.control}
        render={({ field }) => (
          <FormItem>
            <FormLabel>Example Conversation</FormLabel>
            <FormControl>
              <Textarea
                disabled={isLoading}
                rows={7}
                className="bg-background resize-none"
                placeholder={SEED_CHAT}
                {...field}
              />
            </FormControl>
            <FormDescription>
              Write couple of examples of a human chatting with your AI, write
              expected answers.
            </FormDescription>
            <Button
              type="button"
              disabled={isLoading || generatingConversation}
              variant="outline"
              onClick={() => generateConversation()}
            >
              Add Generated Conversation
              {generatingConversation ? (
                <Loader className="w-4 h-4 ml-2 spinner" />
              ) : (
                <Wand2 className="w-4 h-4 ml-2" />
              )}
            </Button>
            <FormMessage />
          </FormItem>
        )}
      />
      <FormField
        name="visibility"
        control={form.control}
        render={({ field }) => (
          <FormItem>
            <FormLabel>Visibility</FormLabel>
            <Select
              disabled={isLoading}
              onValueChange={field.onChange}
              value={field.value}
              defaultValue={field.value}
            >
              <FormControl>
                <SelectTrigger className="bg-background">
                  <SelectValue
                    defaultValue={field.value}
                    placeholder="Select one"
                  />
                </SelectTrigger>
              </FormControl>
              <SelectContent>
                <SelectItem key="PUBLIC" value="PUBLIC">
                  Public
                </SelectItem>
                <SelectItem key="PRIVATE" value="PRIVATE">
                  Private
                </SelectItem>
                <SelectItem key="GROUP" value="GROUP">
                  Group
                </SelectItem>
              </SelectContent>
            </Select>
            <FormDescription>Control who can see your AI</FormDescription>
            <FormMessage />
          </FormItem>
        )}
      />
    </div>
  );
};
