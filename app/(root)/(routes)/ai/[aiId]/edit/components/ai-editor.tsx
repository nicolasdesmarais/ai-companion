"use client";

import LeavePageBlocker from "@/components/leave-page-blocker";
import { Form } from "@/components/ui/form";
import { useToast } from "@/components/ui/use-toast";
import { cn } from "@/src/lib/utils";
import { zodResolver } from "@hookform/resolvers/zod";
import { Category, Knowledge, Prisma } from "@prisma/client";
import axios from "axios";
import { useRouter } from "next/navigation";
import { useEffect, useState } from "react";
import { useForm } from "react-hook-form";
import { AIKnowledge } from "./ai-knowledge";
import { AIPersonality } from "./ai-personality";
import * as z from "zod";
import { AICharacter } from "./ai-character";
import { models } from "./ai-models";
import { Button } from "@/components/ui/button";
const formSchema = z.object({
  id: z.string().optional(),
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
  options: z
    .object({
      maxTokens: z.array(z.number()).optional(),
      temperature: z.array(z.number()).optional(),
      topP: z.array(z.number()).optional(),
      frequencyPenalty: z.array(z.number()).optional(),
      presencePenalty: z.array(z.number()).optional(),
    })
    .transform((value) => value ?? {})
    .optional(),
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
  const [continueRequested, setContinueRequested] = useState(false);

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
      options: {},
    },
  });

  const isLoading = form.formState.isSubmitting;

  useEffect(() => {
    if (Object.keys(form.formState.errors).length) {
      setActiveTab(0);
      console.log(form.formState.errors);
      toast({
        variant: "destructive",
        description: "Form is not valid. Please check the errors.",
        duration: 3000,
      });
    }
  }, [form.formState.errors, toast]);

  const handleTabClick = (index: number) => setActiveTab(index);

  const onContinue = () => {
    const aiId = form.getValues("id");
    setContinueRequested(false);
    if (activeTab === 2) {
      router.push(`/ai/${aiId}`);
    } else {
      setActiveTab(activeTab + 1);
    }
  };

  const onSubmit = async (values: z.infer<typeof formSchema>) => {
    const aiId = form.getValues("id");
    if (form.formState.isDirty) {
      try {
        values.knowledge = values.knowledge?.map((item: any) => item.knowledge);

        let response;
        if (aiId) {
          response = await axios.patch(`/api/v1/ai/${aiId}`, values);
        } else {
          response = await axios.post("/api/v1/ai", values);
        }
        form.reset(response.data);
        toast({
          description: "AI Saved.",
          duration: 2000,
        });
      } catch (error) {
        toast({
          variant: "destructive",
          description: "Something went wrong.",
          duration: 3000,
        });
      }
    }
    if (continueRequested) {
      onContinue();
    }
  };

  const onDelete = async () => {
    const aiId = form.getValues("id");
    if (aiId) {
      try {
        await axios.delete(`/api/v1/ai/${aiId}`);
        toast({
          description: "Deleted Successfully.",
        });
        router.refresh();
        router.push("/");
      } catch (error) {
        toast({
          variant: "destructive",
          description: "Something went wrong.",
        });
      }
    }
  };

  const getContinueButtonText = (form: any) => {
    const needsSave = !form.getValues("id") || form.formState.isDirty;
    if (activeTab === 1) {
      const knowledge = form.getValues("knowledge");
      if (!knowledge.length && !needsSave) {
        return "Skip & Continue";
      }
    }
    if (activeTab === 2) {
      return needsSave
        ? "Save & Start Chatting with your AI"
        : "Start Chatting with your AI";
    }
    return needsSave ? "Save and Continue" : "Continue";
  };

  const getTabClassNames = (isActive: boolean, isDisabled: boolean) => {
    let classNames;
    if (isActive) {
      classNames = "bg-accent text-primary";
    } else if (isDisabled) {
      classNames = "cursor-not-allowed bg-accent/30 text-ring/30";
    } else {
      classNames = "cursor-pointer hover:bg-primary/10";
    }
    return cn(
      "flex grow bg-accent/50 justify-center first:rounded-l-lg last:rounded-r-lg py-4 text-ring transition",
      classNames
    );
  };

  return (
    <div>
      <Form {...form}>
        <form onSubmit={form.handleSubmit(onSubmit)} className="pb-10">
          <div className="flex h-full p-4 space-x-1 max-w-3xl mx-auto">
            {tabs.map((tab, index) => (
              <div
                className={getTabClassNames(
                  activeTab === index,
                  !form.getValues("id") && index > 0
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
              <AICharacter categories={categories} form={form} />
            )}
            {activeTab === 1 && <AIKnowledge form={form} />}
            {activeTab === 2 && (
              <AIPersonality initialAi={initialAi} form={form} />
            )}
          </div>

          <div className="w-full flex justify-between max-w-3xl mx-auto mt-8">
            <div>
              {activeTab === 0 && form.getValues("id") && (
                <Button
                  size="lg"
                  variant="destructive"
                  disabled={isLoading}
                  onClick={onDelete}
                  type="button"
                >
                  Delete your AI
                </Button>
              )}
              {activeTab === 0 && (
                <Button
                  onClick={() => router.push("/")}
                  variant="link"
                  type="button"
                >
                  Back to Browse
                </Button>
              )}
              {activeTab !== 0 && (
                <Button
                  onClick={() => setActiveTab(activeTab - 1)}
                  variant="link"
                  type="button"
                >
                  Go Back
                </Button>
              )}
            </div>
            <div>
              <>
                {form.formState.isDirty || !form.getValues("id") ? (
                  <Button variant="link" disabled={isLoading}>
                    Save Progress
                  </Button>
                ) : null}
                <Button
                  size="lg"
                  variant="ring"
                  disabled={isLoading}
                  onClick={() => setContinueRequested(true)}
                >
                  {getContinueButtonText(form)}
                </Button>
              </>
            </div>
          </div>
        </form>
      </Form>
      <LeavePageBlocker when={form.formState.isDirty} />
    </div>
  );
};
