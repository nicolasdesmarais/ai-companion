"use client";

import * as z from "zod";
import axios, { AxiosError } from "axios";
import { useForm } from "react-hook-form";
import { zodResolver } from "@hookform/resolvers/zod";
import { useRouter } from "next/navigation";
import { Wand2, Loader, FileText } from "lucide-react";
import { Category, Knowledge, Prisma } from "@prisma/client";
import { Form, FormControl, FormDescription, FormField, FormItem, FormLabel, FormMessage } from "@/components/ui/form";
import { Input } from "@/components/ui/input";
import { Textarea } from "@/components/ui/textarea";
import { Button } from "@/components/ui/button";
import { ImageUpload } from "@/components/image-upload";
import { useToast } from "@/components/ui/use-toast";
import { Separator } from "@/components/ui/separator";
import { Select, SelectContent, SelectItem, SelectValue, SelectTrigger } from "@/components/ui/select";
import { useState, useRef } from "react";

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

const formSchema = z.object({
  name: z.string().min(1, {
    message: "Name is required.",
  }),
  description: z.string().min(1, {
    message: "Description is required.",
  }),
  instructions: z.string().min(200, {
    message: "Instructions require at least 200 characters."
  }),
  seed: z.string().min(200, {
    message: "Seed requires at least 200 characters."
  }),
  src: z.string().min(1, {
    message: "Image is required."
  }),
  categoryId: z.string().min(1, {
    message: "Category is required",
  }),
  modelId: z.string().min(1, {
    message: "Model is required",
  }),
});

const models = [
  {
    id: "llama2-13b",
    name: "llama2-13b"
  },
  { 
    id: "gpt-4",
    name: "gpt-4"
  }
];

const supportedUploadFormats = [
  {
    name: "Text",
    type: "text/plain"
  },
  {
    name: "CSV",
    type: "text/csv"
  },
];

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
  initialData: ExtendedCompanion | null;
};

export const CompanionForm = ({
  categories,
  initialData
}: CompanionFormProps) => {
  const { toast } = useToast();
  const router = useRouter();
  const inputFileRef = useRef<HTMLInputElement>(null);
  const [generatingImage, setGeneratingImage] = useState(false);
  const [generatingInstruction, setGeneratingInstruction] = useState(false);
  const [generatingConversation, setGeneratingConversation] = useState(false);
  const initalKnowledge = initialData?.knowledge?.map((item: { knowledge: any }) => item.knowledge) || [];
  const [knowledge, setKnowledge] = useState<Knowledge[]>(initalKnowledge);
  const [uploading, setUploading] = useState(false);

  const form = useForm<z.infer<typeof formSchema>>({
    resolver: zodResolver(formSchema),
    defaultValues: initialData || {
      name: "",
      description: "",
      instructions: "",
      seed: "",
      src: "",
      categoryId: undefined,
      modelId: undefined,
    },
  });

  const isLoading = form.formState.isSubmitting;

  const onSubmit = async (values: z.infer<typeof formSchema>) => {
    try {
      const data = {knowledge, ...values};
      if (initialData) {
        await axios.patch(`/api/companion/${initialData.id}`, data);
      } else {
        await axios.post("/api/companion", data);
      }

      toast({
        description: "Success.",
        duration: 3000,
      });

      router.refresh();
      router.push("/");
    } catch (error) {
      toast({
        variant: "destructive",
        description: "Something went wrong.",
        duration: 3000,
      });
    }
  };

  const generateAvatar = async () => {
    setGeneratingImage(true)
    const name = form.getValues('name');
    const description = form.getValues('description');
    if (name && description) {
      try {
        const response = await axios.post('/api/image', {
          prompt: `Image of ${name}, ${description}`,
          amount: 1,
          resolution: "512x512"
        });
        const urls = response.data.map((image: { url: string }) => image.url);
        form.setValue('src', urls[0])
      } catch (error) {
        toast({
          variant: "destructive",
          description: String((error as AxiosError).response?.data) || "Something went wrong.",
          duration: 6000,
        });
      }
    } else {  
      toast({
        variant: "destructive",
        description: "Name and description are required to generate the avatar.",
        duration: 6000,
      });
    }
    setGeneratingImage(false);
  };

  const generateInstruction = async () => {
    setGeneratingInstruction(true);
    const name = form.getValues('name');
    const description = form.getValues('description');
    if (name && description) {
      try {
        const response = await axios.post('/api/generate', {
          prompt: `Generate an AI agent prompt for ${name}, ${description}.  Prompt should be at least 200 characters long.`,
        });
        form.setValue('instructions', response.data)
      } catch (error: any) {
        toast({
          variant: "destructive",
          description: String((error as AxiosError).response?.data) || "Something went wrong.",
          duration: 6000,
        });
      }
    } else {  
      toast({
        variant: "destructive",
        description: "Name and description are required to generate the instruction.",
        duration: 3000,
      });
    }
    setGeneratingInstruction(false);
  };

  const generateConversation = async () => {
    setGeneratingConversation(true);
    const name = form.getValues('name');
    const description = form.getValues('description');
    const instructions = form.getValues('instructions');
    const seed = form.getValues('seed');
    if (name && description) {
      try {
        let history;
        if (!seed) {
          history = `Human: Hi ${name}\n`;
        } else {
          const question = await axios.post('/api/generate', {
            prompt: `
              Pretend you are a human talking to an AI agent ${name}, ${description}.  Continue the conversation below.\n\n
              ${seed}\nHuman:
            `,
          });
          history = `${seed}Human: ${question.data}\n`;
        }
        const response = await axios.post('/api/generate', {
          prompt: `
          ONLY generate plain sentences without prefix of who is speaking. DO NOT use ${name}: prefix. 

          ${instructions}
  
          Below are relevant details about ${name}'s past and the conversation you are in.
          ${history}\n${name}:`,
        });
        form.setValue('seed', `${history}\n${name}: ${response.data}\n\n`)
      } catch (error: any) {
        toast({
          variant: "destructive",
          description: String((error as AxiosError).response?.data) || "Something went wrong.",
          duration: 6000,
        });
      }
    } else {  
      toast({
        variant: "destructive",
        description: "Name, description and instructions are required to generate the conversation.",
        duration: 6000,
      });
    }
    setGeneratingConversation(false);
  };

  const uploadDocument = async () => {
    setUploading(true);
    if (!inputFileRef.current?.files || inputFileRef.current?.files.length === 0) {
      toast({
        variant: "destructive",
        description: "No file selected.",
        duration: 6000,
      });
      return;
    }
    const file = inputFileRef.current.files[0];
    if (supportedUploadFormats.findIndex((format) => format.type === file.type) === -1) {
      toast({
        variant: "destructive",
        description: "This file format is not supported",
        duration: 6000,
      });
    }
    try {
      const response = await axios.post(
        `/api/knowledge?filename=${file.name}&type=${file.type}`,
        file,
      );
      setKnowledge((current) => [...current, response.data]);
    } catch (error: any) {
      toast({
        variant: "destructive",
        description: String((error as AxiosError).response?.data) || "Something went wrong.",
        duration: 6000,
      });
    }
    setUploading(false);
  };

  return ( 
    <div className="h-full p-4 space-y-2 max-w-3xl mx-auto">
      <Form {...form}>
        <form onSubmit={form.handleSubmit(onSubmit)} className="space-y-8 pb-10">
          <div className="space-y-2 w-full col-span-2">
            <div>
              <h3 className="text-lg font-medium">General Information</h3>
              <p className="text-sm text-muted-foreground">
                General information about your Companion
              </p>
            </div>
            <Separator className="bg-primary/10" />
          </div>
          <FormField
            name="src"
            render={({ field }) => (
              <FormItem className="flex flex-col items-center justify-center space-y-4 col-span-2">
                <FormControl>
                  <ImageUpload disabled={isLoading} onChange={field.onChange} value={field.value} />
                </FormControl>
                <Button type="button" disabled={isLoading || generatingImage} variant="outline" onClick={() => generateAvatar()}>
                  Generate Avatar Image
                  {generatingImage ? <Loader className="w-4 h-4 ml-2 spinner"/> : <Wand2 className="w-4 h-4 ml-2" />}
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
                    <Input disabled={isLoading} placeholder="Elon Musk" {...field} />
                  </FormControl>
                  <FormDescription>
                    This is how your AI Companion will be named.
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
                    <Input disabled={isLoading} placeholder="CEO & Founder of Tesla, SpaceX" {...field} />
                  </FormControl>
                  <FormDescription>
                    Short description for your AI Companion
                  </FormDescription>
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
                  <Select disabled={isLoading} onValueChange={field.onChange} value={field.value} defaultValue={field.value}>
                    <FormControl>
                      <SelectTrigger className="bg-background">
                        <SelectValue defaultValue={field.value} placeholder="Select a category" />
                      </SelectTrigger>
                    </FormControl>
                    <SelectContent>
                      {categories.map((category) => (
                        <SelectItem key={category.id} value={category.id}>{category.name}</SelectItem>
                      ))}
                    </SelectContent>
                  </Select>
                  <FormDescription>
                    Select a category for your AI
                  </FormDescription>
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
                  <Select disabled={isLoading} onValueChange={field.onChange} value={field.value} defaultValue={field.value}>
                    <FormControl>
                      <SelectTrigger className="bg-background">
                        <SelectValue defaultValue={field.value} placeholder="Select a model" />
                      </SelectTrigger>
                    </FormControl>
                    <SelectContent>
                      {models.map((model) => (
                        <SelectItem key={model.id} value={model.id}>{model.name}</SelectItem>
                      ))}
                    </SelectContent>
                  </Select>
                  <FormDescription>
                    Select a LLM for your AI
                  </FormDescription>
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
                  <Textarea disabled={isLoading} rows={7} className="bg-background resize-none" placeholder={PREAMBLE} {...field} />
                </FormControl>
                <FormDescription>
                  Describe in detail your companion&apos;s backstory and relevant details.
                </FormDescription>
                <Button type="button" disabled={isLoading || generatingInstruction} variant="outline" onClick={() => generateInstruction()}>
                  Generate Instruction
                  {generatingInstruction ? <Loader className="w-4 h-4 ml-2 spinner"/> : <Wand2 className="w-4 h-4 ml-2" />}
                </Button>
                <FormMessage />
              </FormItem>
            )}
          />
          <div>
            <FormItem>
              <FormLabel>Custom Knowledge</FormLabel>
              <div>
                <div>
                  {knowledge.map((item) => (
                    <div key={item.id} className="flex items-center justify-between w-full bg-background rounded-lg p-2 my-2">
                      <p className="text-sm px-3 py-2">{item.name}</p>
                      <Button type="button" variant="outline" onClick={() => setKnowledge((current) => current.filter((i) => i.id !== item.id))}>
                        Remove
                      </Button>
                    </div>
                  ))}
                </div>
                <div className="flex">
                  <Input name="file" ref={inputFileRef} type="file" />
                  <Button type="button" disabled={isLoading || uploading} variant="outline" onClick={() => uploadDocument()}>
                    Upload
                    {uploading ? <Loader className="w-4 h-4 ml-2 spinner"/> : <FileText className="w-4 h-4 ml-2" />}
                  </Button>
                </div>
              </div>
              <FormDescription>
                Add custom knowledge to your AI. Max file size: 4.5Mb. The following formats are supported: {supportedUploadFormats.map((format) => format.name).join(", ")}
              </FormDescription>
              <FormMessage />
            </FormItem>
          </div>
          <FormField
            name="seed"
            control={form.control}
            render={({ field }) => (
              <FormItem>
                <FormLabel>Example Conversation</FormLabel>
                <FormControl>
                  <Textarea disabled={isLoading} rows={7} className="bg-background resize-none" placeholder={SEED_CHAT} {...field} />
                </FormControl>
                <FormDescription>
                  Write couple of examples of a human chatting with your AI companion, write expected answers.
                </FormDescription>
                <Button type="button" disabled={isLoading || generatingConversation} variant="outline" onClick={() => generateConversation()}>
                  Add Generated Conversation
                  {generatingConversation ? <Loader className="w-4 h-4 ml-2 spinner"/> : <Wand2 className="w-4 h-4 ml-2" />}
                </Button>
                <FormMessage />
              </FormItem>
            )}
          />
          <div className="w-full flex justify-center">
            <Button size="lg" disabled={isLoading}>
              {initialData ? "Save your companion" : "Create your companion"}
              <Wand2 className="w-4 h-4 ml-2" />
            </Button>
          </div>
        </form>
      </Form>
    </div>
   );
};