"use client";

import LeavePageBlocker from "@/components/leave-page-blocker";
import { Button } from "@/components/ui/button";
import { Form } from "@/components/ui/form";
import { useToast } from "@/components/ui/use-toast";
import { AIModel } from "@/src/domain/models/AIModel";
import { cn } from "@/src/lib/utils";
import { zodResolver } from "@hookform/resolvers/zod";
import { Category, Group, Knowledge, Prisma } from "@prisma/client";
import axios from "axios";
import { usePathname, useRouter } from "next/navigation";
import { useEffect, useState } from "react";
import { useForm } from "react-hook-form";
import * as z from "zod";
import { AICharacter } from "./ai-character";
import { AIKnowledge } from "./ai-knowledge";
import { AIPersonality } from "./ai-personality";

const formSchema = z.object({
  id: z.string().optional(),
  name: z.string().min(1, {
    message: "Name is required.",
  }),
  description: z.string().min(1, {
    message: "Description is required.",
  }),
  instructions: z.string().min(100, {
    message: "Instructions require at least 100 characters.",
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
  groups: z.array(z.string()).optional(),
  talk: z.string().optional(),
});

const extendedAI = Prisma.validator<Prisma.AIDefaultArgs>()({
  include: {
    dataSources: {
      include: {
        dataSource: true,
      },
    },
  },
});

type ExtendedAI = Prisma.AIGetPayload<typeof extendedAI>;

interface AIFormProps {
  aiModels: AIModel[];
  categories: Category[];
  initialAi: ExtendedAI | null;
  groups: Group[];
}

export const AIEditor = ({
  aiModels,
  categories,
  initialAi,
  groups,
}: AIFormProps) => {
  const { toast } = useToast();
  const router = useRouter();
  const pathname = usePathname();
  const [continueRequested, setContinueRequested] = useState("");
  const [dataSources, setDataSources] = useState<any[]>([]);
  const [dataSourcesLoading, setDataSourcesLoading] = useState(true);

  useEffect(() => {
    const fetchDataSources = async () => {
      setDataSourcesLoading(true);
      const response = await axios.get(`/api/v1/ai/${aiId}/data-sources`);
      setDataSources(response.data.data);
      setDataSourcesLoading(false);
    };
    fetchDataSources();
  }, []);

  if (initialAi && !initialAi.options) {
    const model = aiModels.find((model) => model.id === initialAi.modelId);
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
    defaultValues: initialAi
      ? ({ talk: "", ...initialAi } as any)
      : {
          name: "",
          description: "",
          instructions: "",
          seed: "",
          src: "",
          categoryId: undefined,
          modelId: "gpt-4",
          visibility: "ORGANIZATION",
          knowledge: [],
          options: {},
          groups: [],
        },
  });

  const aiId = form.getValues("id");
  const isLoading = form.formState.isSubmitting;
  const needsSave = !aiId || form.formState.isDirty;

  useEffect(() => {
    if (Object.keys(form.formState.errors).length) {
      toast({
        variant: "destructive",
        description: "Form is not valid. Please check the errors.",
        duration: 3000,
      });
    }
  }, [form.formState.errors, toast]);

  const handleTabClick = (route: string, isDisabled: boolean) => {
    if (!isDisabled) {
      router.push(`/ai/${aiId}/${route}` as any);
    }
  };

  const onSubmit = async (values: z.infer<typeof formSchema>) => {
    let aiId = form.getValues("id");
    if (form.formState.isDirty) {
      try {
        let response;
        if (aiId) {
          response = await axios.patch(`/api/v1/ai/${aiId}`, values);
        } else {
          response = await axios.post("/api/v1/ai", values);
        }
        aiId = response.data.id;
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
      setContinueRequested("");
      router.push(`/ai/${aiId}${continueRequested}`);
    } else {
      router.push(`/ai/${aiId}/edit`);
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

  const saveProgressButton =
    form.formState.isDirty || !form.getValues("id") ? (
      <Button variant="link" disabled={isLoading}>
        Save Progress
      </Button>
    ) : null;

  const continueButton = (route: string, text: string) => (
    <Button
      size="lg"
      variant="ring"
      disabled={isLoading}
      onClick={() => setContinueRequested(route)}
    >
      {text}
    </Button>
  );

  const backButton = (route: string) => (
    <Button
      onClick={() => router.push(`/ai/${aiId}/${route}`)}
      variant="link"
      type="button"
    >
      Go Back
    </Button>
  );

  const aiKnowledge = (
    <AIKnowledge
      form={form}
      dataSources={dataSources}
      setDataSource={setDataSources}
      knowledgeLoading={dataSourcesLoading}
    />
  );

  const tabs = [
    {
      name: "Character",
      index: 1,
      route: "edit",
      content: (
        <AICharacter
          aiModels={aiModels}
          categories={categories}
          form={form}
          groups={groups}
        />
      ),
      buttons: (
        <>
          <div>
            {aiId && (
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
            <Button
              onClick={() => router.push("/")}
              variant="link"
              type="button"
              disabled={isLoading}
            >
              Back to Browse
            </Button>
          </div>
          <div>
            {saveProgressButton}
            {continueButton(
              "/edit/knowledge",
              needsSave ? "Save and Continue" : "Continue"
            )}
          </div>
        </>
      ),
    },
    {
      name: "Knowledge",
      route: "edit/knowledge",
      index: 2,
      content: aiKnowledge,
      buttons: (
        <>
          <div>{backButton("edit")}</div>
          <div>
            {saveProgressButton}
            {continueButton(
              "/edit/personality",
              dataSources.length ? "Continue" : "Skip & Continue"
            )}
          </div>
        </>
      ),
    },
    {
      name: "File Upload",
      secondary: true,
      route: "edit/knowledge/file",
      content: aiKnowledge,
      buttons: (
        <>
          <div>{backButton("edit/knowledge")}</div>
          <div>
            {saveProgressButton}
            {continueButton(
              "/edit/personality",
              dataSources.length ? "Continue" : "Skip & Continue"
            )}
          </div>
        </>
      ),
    },

    {
      name: "Website URL",
      secondary: true,
      route: "edit/knowledge/web-url",
      content: aiKnowledge,
      buttons: (
        <>
          <div>{backButton("edit/knowledge")}</div>
          <div>
            {saveProgressButton}
            {continueButton(
              "/edit/personality",
              dataSources.length ? "Continue" : "Skip & Continue"
            )}
          </div>
        </>
      ),
    },
    {
      name: "Cloud Storage",
      secondary: true,
      route: "edit/knowledge/cloud",
      content: aiKnowledge,
      buttons: (
        <>
          <div>{backButton("edit/knowledge")}</div>
          <div>
            {saveProgressButton}
            {continueButton(
              "/edit/personality",
              dataSources.length ? "Continue" : "Skip & Continue"
            )}
          </div>
        </>
      ),
    },
    {
      name: "Personality",
      route: "edit/personality",
      index: 3,
      content: (
        <AIPersonality initialAi={initialAi} form={form} aiModels={aiModels} />
      ),
      buttons: (
        <>
          <div>{backButton("edit/knowledge")}</div>
          <div>
            {saveProgressButton}
            {continueButton(
              "/",
              needsSave
                ? "Save & Start Chatting with your AI"
                : "Start Chatting with your AI"
            )}
          </div>
        </>
      ),
    },
  ];

  const activeTab = tabs.find((tab) => pathname.endsWith(tab.route));

  return (
    <div>
      <Form {...form}>
        <form onSubmit={form.handleSubmit(onSubmit)} className="pb-10">
          <div className="flex h-full p-4 space-x-1 max-w-3xl mx-auto">
            {tabs.map((tab, index) =>
              tab.secondary ? null : (
                <div
                  className={getTabClassNames(
                    pathname.endsWith(tab.route),
                    !aiId && index > 0
                  )}
                  key={index}
                  onClick={() => handleTabClick(tab.route, !aiId && index > 0)}
                >
                  <div className="bg-secondary rounded-lg px-2 text-ring">
                    {tab.index}
                  </div>
                  <div className="ml-2">{tab.name}</div>
                </div>
              )
            )}
          </div>
          <div>{activeTab?.content}</div>
          <div className="w-full flex justify-between max-w-3xl mx-auto mt-8">
            {activeTab?.buttons}
          </div>
        </form>
      </Form>
      <LeavePageBlocker when={form.formState.isDirty} />
    </div>
  );
};
