"use client";

import LeavePageBlocker from "@/components/leave-page-blocker";
import { Button } from "@/components/ui/button";
import { Form } from "@/components/ui/form";
import { useToast } from "@/components/ui/use-toast";
import { AIDetailDto } from "@/src/domain/models/AI";
import { AIModel } from "@/src/domain/models/AIModel";
import { cn } from "@/src/lib/utils";
import { zodResolver } from "@hookform/resolvers/zod";
import { Knowledge } from "@prisma/client";
import axios from "axios";
import { usePathname, useRouter } from "next/navigation";
import { useEffect, useState } from "react";
import { useForm } from "react-hook-form";
import * as z from "zod";
import { AICharacter } from "./ai-character";
import { AIKnowledge } from "./ai-knowledge";
import { AIPersonality } from "./ai-personality";
import { AIProfileEditor } from "./ai-profile-editor";
import { PaywallBanner } from "./paywall-banner";

const formSchema = z.object({
  id: z.string().optional(),
  name: z.string().min(1, {
    message: "Name is required.",
  }),
  introduction: z.string().optional().nullable(),
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
  categories: z.array(z.string()).optional(),
  modelId: z.string().min(1, {
    message: "Model is required",
  }),
  visibility: z.string().min(1, {
    message: "Visibility is required",
  }),
  listInOrgCatalog: z.boolean().optional(),
  listInPublicCatalog: z.boolean().optional(),
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
  profile: z
    .object({
      headline: z.string().optional().nullable(),
      description: z.string().optional().nullable(),
      features: z
        .array(
          z.object({
            title: z.string().optional().nullable(),
            description: z.string().optional().nullable(),
          })
        )
        .optional()
        .nullable(),
      conversations: z
        .array(
          z.object({
            messages: z.array(
              z.object({
                role: z.string(),
                content: z.string(),
              })
            ),
          })
        )
        .optional()
        .nullable(),
      trainingDescription: z.string().optional().nullable(),
      showCharacter: z.boolean().optional().nullable(),
      showTraining: z.boolean().optional().nullable(),
      showPersonality: z.boolean().optional().nullable(),
      socialImage: z.string().optional().nullable(),
    })
    .optional()
    .nullable(),
});

const defaultProfile = {
  headline: undefined,
  description: undefined,
  features: [],
  showCharacter: undefined,
  showTraining: undefined,
  showPersonality: undefined,
  trainingDescription: undefined,
  conversations: undefined,
  socialImage: undefined,
};

interface AIFormProps {
  aiModels: AIModel[];
  initialAi: AIDetailDto | null;
  hasInstanceAccess: boolean;
}

export const AIEditor = ({
  aiModels,
  initialAi,
  hasInstanceAccess,
}: AIFormProps) => {
  const { toast } = useToast();
  const router = useRouter();
  const pathname = usePathname();
  const [continueRequested, setContinueRequested] = useState("");
  const [dataSources, setDataSources] = useState<any[]>([]);
  const [dataSourcesLoading, setDataSourcesLoading] = useState(true);

  const fetchDataSources = async () => {
    setDataSourcesLoading(true);
    const response = await axios.get(`/api/v1/ai/${aiId}/data-sources`);
    setDataSources(response.data.data);
    setDataSourcesLoading(false);
  };

  useEffect(() => {
    if (aiId) {
      fetchDataSources();
    }
  }, []);

  if (initialAi) {
    const model = aiModels.find((model) => model.id === initialAi.modelId);
    if (model) {
      const options = {} as any;
      Object.entries(model.options).forEach(([key, value]) => {
        if (value.default !== undefined) {
          options[key] = [value.default];
        }
      });
      if (initialAi.options) {
        initialAi.options = { ...options, ...(initialAi.options as any) };
      } else {
        initialAi.options = options;
      }
      if (initialAi.profile) {
        initialAi.profile = { ...defaultProfile, ...initialAi.profile };
      } else {
        initialAi.profile = defaultProfile;
      }
    }
  }

  const form = useForm<z.infer<typeof formSchema>>({
    resolver: zodResolver(formSchema),
    defaultValues: initialAi
      ? ({ talk: "", ...initialAi } as any)
      : {
          name: "",
          introduction: "",
          description: "",
          instructions: "",
          seed: "",
          src: "",
          categories: [],
          modelId: "gpt-4",
          visibility: "ANYONE_WITH_LINK",
          listInOrgCatalog: false,
          listInPublicCatalog: false,
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
      if (form.formState.isDirty) {
        toast({
          variant: "destructive",
          description: "Changes detected. Please save to continue.",
          duration: 3000,
        });
      } else {
        router.push(`/ai/${aiId}/${route}` as any);
      }
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
        if (response.data.profile) {
          response.data.profile = {
            ...defaultProfile,
            ...response.data.profile,
          };
        } else {
          response.data.profile = defaultProfile;
        }
        form.reset({ talk: "", ...response.data }); //TODO: remove talk
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
      if (pathname.endsWith("/new/edit")) {
        router.push(`/ai/${aiId}/edit`);
      } else {
        router.push(pathname);
      }
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
      onClick={() => handleTabClick(route, false)}
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
      aiModels={aiModels}
      fetchDataSources={fetchDataSources}
    />
  );

  const tabs = [
    {
      name: "Character",
      index: 1,
      route: "edit",
      content: (
        <AICharacter
          form={form}
          hasInstanceAccess={hasInstanceAccess}
          save={form.handleSubmit(onSubmit)}
        />
      ),
      buttons: (
        <>
          <div className="flex flex-col md:flex-row items-center">
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
          <div className="flex flex-col md:flex-row items-center">
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
          <div className="flex flex-col md:flex-row items-center">
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
      name: "Link Existing Knowledge",
      secondary: true,
      route: "edit/knowledge/connect",
      content: aiKnowledge,
      buttons: (
        <>
          <div>{backButton("edit/knowledge")}</div>
          <div className="flex flex-col md:flex-row items-center">
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
          <div className="flex flex-col md:flex-row items-center">
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
          <div className="flex flex-col md:flex-row items-center">
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
      name: "Google Drive Storage",
      secondary: true,
      route: "edit/knowledge/google-drive",
      content: aiKnowledge,
      buttons: (
        <>
          <div>{backButton("edit/knowledge")}</div>
          <div className="flex flex-col md:flex-row items-center">
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
      name: "OneDrive Storage",
      secondary: true,
      route: "edit/knowledge/one-drive",
      content: aiKnowledge,
      buttons: (
        <>
          <div>{backButton("edit/knowledge")}</div>
          <div className="flex flex-col md:flex-row items-center">
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
          <div className="flex flex-col md:flex-row items-center">
            {saveProgressButton}
            {continueButton(
              "/edit/profile",
              needsSave ? "Save & Continue" : "Continue"
            )}
          </div>
        </>
      ),
    },
    {
      name: "Profile",
      route: "edit/profile",
      index: 4,
      content: <AIProfileEditor ai={initialAi} form={form} />,
      buttons: (
        <>
          <div>{backButton("edit/knowledge")}</div>
          <div className="flex flex-col md:flex-row items-center">
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
    <div className="pt-14 md:pt-0">
      <PaywallBanner className="mt-2 max-w-3xl mx-auto" />
      <Form {...form}>
        <form onSubmit={form.handleSubmit(onSubmit)} className="pb-10">
          <div className="flex h-full p-1 md:p-4 space-x-1 max-w-3xl mx-auto">
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
                  <div className="bg-secondary rounded-lg px-2 text-ring hidden md:block">
                    {tab.index}
                  </div>
                  <div className="ml-0 md:ml-2">{tab.name}</div>
                </div>
              )
            )}
          </div>
          <div>{activeTab?.content}</div>
          <div className="w-full flex flex-col md:flex-row items-center justify-between max-w-3xl mx-auto mt-8">
            {form.getValues("id") ||
            (form.getValues("instructions") && form.getValues("src"))
              ? activeTab?.buttons
              : null}
          </div>
        </form>
      </Form>
      <LeavePageBlocker when={form.formState.isDirty} />
    </div>
  );
};
