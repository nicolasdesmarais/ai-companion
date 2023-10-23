"use client";

import { Category, Prisma, Knowledge } from "@prisma/client";
import { AICharacter } from "./ai-character";
import { useEffect, useState } from "react";
import { cn } from "@/lib/utils";
import { AIKnowledge } from "./ai-knowledge";
import { AIPersonality } from "./ai-personality";
import { useForm } from "react-hook-form";
import * as z from "zod";
import { zodResolver } from "@hookform/resolvers/zod";
import axios from "axios";
import { useToast } from "@/components/ui/use-toast";
import { useRouter } from "next/navigation";
import { Form } from "@/components/ui/form";
import LeavePageBlocker from "@/components/leave-page-blocker";
import { models } from "./ai-models";

const formSchema = z.object({
  name: z.string().min(1, {
    message: "Name is required.",
  }),
  description: z.string().min(1, {
    message: "Description is required.",
  }),
  instructions: z.string().min(200, {
    message: "Instructions require at least 200 characters.",
  }),
  seed: z.string().optional(),
  src: z.string().min(1, {
    message: "Image is required.",
  }),
  categoryId: z.string().min(1, {
    message: "Category is required",
  }),
  modelId: z.string().min(1, {
    message: "Model is required",
  }),
  visibility: z.string().min(1, {
    message: "Visibility is required",
  }),
  options: z.object({
    max_tokens: z.array(z.number()).optional(),
    temperature: z.array(z.number()).optional(),
    top_p: z.array(z.number()).optional(),
    frequency_penalty: z.array(z.number()).optional(),
    presence_penalty: z.array(z.number()).optional(),
  }),
  knowledge: z.array(z.custom<Knowledge>()).optional(),
});

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
  initialAi: ExtendedCompanion | null;
}

const tabs = ["Character", "Knowledge", "Personality"];

export const AIEditor = ({ categories, initialAi }: CompanionFormProps) => {
  const { toast } = useToast();
  const router = useRouter();
  const [activeTab, setActiveTab] = useState(0);
  if (initialAi && !initialAi.options) {
    const model = models.find((model) => model.id === initialAi.modelId);
    if (model) {
      const options = {} as any;
      Object.entries(model.options).forEach(([key, value]) => {
        if (value.default) {
          options[key] = [value.default];
        }
      });
      initialAi.options = options;
    }
  }
  const form = useForm<z.infer<typeof formSchema>>({
    resolver: zodResolver(formSchema),
    defaultValues: (initialAi as any) || {
      name: "",
      description: "",
      instructions: "",
      seed: "",
      src: "",
      categoryId: undefined,
      modelId: "gpt-4",
      knowledge: [],
    },
  });
  useEffect(() => {
    if (Object.keys(form.formState.errors).length) {
      setActiveTab(0);
      toast({
        variant: "destructive",
        description: "Form is not valid. Please check the errors.",
        duration: 3000,
      });
    }
  }, [form.formState.errors, toast]);

  const handleTabClick = (index: number) => setActiveTab(index);

  const onSubmit = async (values: z.infer<typeof formSchema>) => {
    try {
      values.knowledge = values.knowledge?.map((item: any) => item.knowledge);

      if (initialAi) {
        await axios.patch(`/api/v1/ai/${initialAi.id}`, values);
      } else {
        await axios.post("/api/v1/ai", values);
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

  return (
    <div>
      <Form {...form}>
        <form onSubmit={form.handleSubmit(onSubmit)} className="pb-10">
          <div className="flex h-full p-4 space-x-1 max-w-3xl mx-auto">
            {tabs.map((tab, index) => (
              <div
                className={cn(
                  "flex grow bg-accent/50 justify-center first:rounded-l-lg last:rounded-r-lg py-4 text-ring transition",
                  activeTab === index
                    ? "bg-accent text-primary"
                    : "cursor-pointer hover:bg-primary/10"
                )}
                key={index}
                onClick={() => handleTabClick(index)}
              >
                <div className="bg-secondary rounded-lg px-2 text-ring">
                  {index + 1}
                </div>
                <div className="ml-2">{tab}</div>
              </div>
            ))}
          </div>
          <div>
            {activeTab === 0 && (
              <AICharacter
                initialData={initialAi}
                categories={categories}
                form={form}
              />
            )}
            {activeTab === 1 && <AIKnowledge aiId={initialAi?.id} />}
            {activeTab === 2 && (
              <AIPersonality initialAi={initialAi} form={form} />
            )}
          </div>
        </form>
      </Form>
      <LeavePageBlocker when={form.formState.isDirty} />
    </div>
  );
};
