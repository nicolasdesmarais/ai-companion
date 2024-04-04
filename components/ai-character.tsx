"use client";

import { ImageUpload } from "@/components/image-upload";
import { Button } from "@/components/ui/button";
import { Checkbox } from "@/components/ui/checkbox";
import { Dialog, DialogContent } from "@/components/ui/dialog";
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
import { useGroupModal } from "@/hooks/use-group-modal";
import { useTalkModal } from "@/hooks/use-talk-modal";
import { GroupSummaryDto } from "@/src/domain/models/Groups";
import { getDiversityString } from "@/src/lib/diversity";
import {AIVisibility, Category, DataSourceRefreshPeriod} from "@prisma/client";
import axios, { AxiosError } from "axios";
import { Loader, Play, Settings, Wand2 } from "lucide-react";
import { usePathname } from "next/navigation";
import { useEffect, useState } from "react";
import { imageModels, voices } from "./ai-models";
import { TalkModal } from "./talk-modal";
import {MultiSelect} from "@/components/ui/multi-select";
import {CategoryTypes} from "@/components/category-types";

const PREAMBLE =
  "ex: As a Support Specialist AI, your role is to provide solutions to user inquiries. This involves understanding the nature of the questions, interpreting their context, and yielding the correct responses. The effectiveness of your position is evaluated based on the accuracy of your responses and the satisfaction of users. Precision in answering and user satisfaction are your primary goals.";

const SEED_CHAT = `example:

Human: Hello, I'm having some difficulty navigating AppDirect's marketplace, can you assist me?

Support Specialist AI: Of course, I'd be happy to assist. Could you please tell me specifically what you're having trouble with on the marketplace?

Human: I'm trying to find the knowledge base, but I'm not sure where to look.

Support Specialist AI: You can find the knowledge base on the top menu of the AppDirect homepage. Here, you'll find a range of articles and guides that can help you understand and make the most of our marketplace.

Human: Alright, found it! But, I'm still a bit confused about using it effectively. Are there any specific guides you would recommend for a beginner?

Support Specialist AI: Absolutely. Under the section entitled "Getting Started", there are several step-by-step guides that a majority of our new users find very helpful. These guides provide detailed explanations and pictures that can assist you in understanding the marketplace better.

Human: Got it. Thanks for your assistance.

Support Specialist AI: You're welcome! Feel free to ask if you have any further questions. I'm here to help!
`;

const INSTRUCTION_PROMPT = `Create a single short paragraph description of an AI you are training to answer questions from users.
Write it as if you are explaining to them their job, role and responsibilities. Write it simply and direct without excessive adjectives or niceties.
Do not mention continuous learning. Write the paragraph based on the following information about them: `;

const loadingMessages = [
  "Building the AI...",
  "Adding the brain...",
  "Inserting electrodes...",
  "Turning on the power...",
  "Training the AI...",
  "Adding the three laws of robotics...",
  "Creating an avatar...",
  "Generating the personality...",
  "Setting the honesty parameters...",
];

interface AIFormProps {
  form: any;
  hasInstanceAccess: boolean;
  save: () => void;
}

export const AICharacter = ({ form, hasInstanceAccess, save }: AIFormProps) => {
  const { toast } = useToast();
  const groupModal = useGroupModal();
  const talkModal = useTalkModal();
  const pathname = usePathname();
  const [generatingImage, setGeneratingImage] = useState(false);
  const [generatingInstruction, setGeneratingInstruction] = useState(false);
  const [generatingConversation, setGeneratingConversation] = useState(false);
  const [groupList, setGroupList] = useState<GroupSummaryDto[]>([]);
  const [advancedImage, setAdvancedImage] = useState(false);
  const [imagePrompt, setImagePrompt] = useState("");
  const [imageModel, setImageModel] = useState("latent-consistency");
  const [diversityString, setDiversityString] = useState("");
  const [categories, setCategories] = useState<Category[]>([]);
  const [generatingAll, setGeneratingAll] = useState(false);
  const [loadingMessage, setLoadingMessage] = useState(loadingMessages[0]);
  const [selectedValues, setSelectedValues] = useState<any[]>([]);
  const isLoading = form.formState.isSubmitting;
  const voiceEnabled = false && window.location.hostname !== "appdirect.ai";

  const onSubmit = async (values: any) => {}

  const fetchGroups = async () => {
    const response = await axios.get("/api/v1/me/groups");
    if (response.status === 200 && Array.isArray(response.data.data)) {
      setGroupList(response.data.data);
    }
  };

  const fetchCategories = async () => {
    const response = await axios.get("/api/v1/categories");
    if (response.status === 200 && Array.isArray(response.data)) {
      setCategories(response.data);
    }
  };

  useEffect(() => {
    fetchGroups();
    fetchCategories();
    const interval = setInterval(() => {
      setLoadingMessage((msg) => {
        const thisIndex = loadingMessages.indexOf(msg);
        return loadingMessages[
          thisIndex + 1 === loadingMessages.length ? 0 : thisIndex + 1
        ];
      });
    }, 3000);

    return () => clearInterval(interval);
  }, []);

  useEffect(() => {
    if (generatingAll && !pathname.endsWith("/new/edit")) {
      setGeneratingAll(false);
    }
  }, [pathname, generatingAll]);

  useEffect(() => {
    if (groupModal.areGroupsUpdated) {
      fetchGroups();
    }
  }, [groupModal.areGroupsUpdated]);

  useEffect(() => {
    if (!advancedImage) {
      const name = form.getValues("name");
      const description = form.getValues("description");
      if (name && description) {
        setImagePrompt(
          `${name}, ${description}: ${
            diversityString || getDiversityString()
          }. photorealistic portrait. family friendly. shot of the sony a7rv 35mm f1.8. HDR. 4k.`
        );
      }
    }
  }, [
    advancedImage,
    form.getValues("name"),
    form.getValues("description"),
    diversityString,
  ]);

  useEffect(() => {
    if (voiceEnabled && form.getValues("src")) {
      setupTalk();
    }
  }, [form.getValues("src")]);

  const setupTalk = async () => {
    form.setValue("talk", null, {
      shouldDirty: true,
    });
    const create = await axios.post("/api/v1/talk", {
      prompt: `Hello, I am ${form.getValues("name")}`,
      imgUrl: form.getValues("src"),
    });
    if (create.data.id) {
      const talk = await axios.get(`/api/v1/talk/${create.data.id}`);
      if (talk.data.status !== "error") {
        form.setValue("talk", talk.data.id, {
          shouldDirty: true,
        });
      }
    }
  };

  const playTalk = async () => {
    const talkId = form.getValues("talk");
    if (talkId) {
      const talk = await axios.get(`/api/v1/talk/${talkId}`);
      if (talk.data.status === "done") {
        talkModal.onOpen(talk.data.result_url);
      }
    }
  };

  const generateAvatar = async () => {
    setGeneratingImage(true);
    if (imagePrompt) {
      try {
        const response = await axios.post("/api/v1/image", {
          prompt: imagePrompt,
          model: imageModel,
        });
        form.setValue("src", response.data.secure_url, { shouldDirty: true });
      } catch (error) {
        console.log(error);
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
          "Name and description or custom prompt is required to generate the avatar.",
        duration: 6000,
      });
    }
    setDiversityString(getDiversityString());
    setGeneratingImage(false);
  };

  const generateInstruction = async () => {
    setGeneratingInstruction(true);
    const name = form.getValues("name");
    const description = form.getValues("description");
    if (name && description) {
      try {
        const response = await axios.post("/api/v1/generate", {
          prompt: `${INSTRUCTION_PROMPT} ${name}, ${description}.`,
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
        description: "Fill out the name and description to generate your AI.",
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
    if (name && description) {
      try {
        const response = await axios.post("/api/v1/generate", {
          prompt: `Create an example conversation between a Human and ${name} where the Human is asking questions of the AI named ${name}.
          Create the conversation based on what a user would ask an AI who is trained on the following information: ${instructions}`,
        });
        form.setValue("seed", response.data, { shouldDirty: true });
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
          "Fill out the name, description and instructions to generate training materials.",
        duration: 6000,
      });
    }
    setGeneratingConversation(false);
  };

  const generateAll = async () => {
    setGeneratingAll(true);
    const defaultCat = categories[0];
    form.setValue("categoryId", defaultCat.id, { shouldDirty: true });
    form.setValue("introduction", `How may I be of assistance today?`, {
      shouldDirty: true,
    });
    await Promise.all([generateAvatar(), generateInstruction()]);
    save();
  };

  return (
    <div className="h-full p-4 space-y-8 max-w-3xl mx-auto">
      <div className="space-y-2 w-full">
        <div>
          <h3 className="text-lg font-medium">
            Create an AI app in 1 easy step
          </h3>
          <p className="text-sm text-muted-foreground">
            Auto generate your app, then tweak the information for better
            results.
          </p>
        </div>
        <Separator className="bg-primary/10" />
      </div>
      <FormField
        name="name"
        control={form.control}
        render={({ field }) => (
          <FormItem className="">
            <FormLabel>
              Create a name that helps to represent your AIs purpose.
            </FormLabel>
            <FormControl>
              <Input
                disabled={isLoading}
                placeholder="ex: Support Specialist"
                {...field}
              />
            </FormControl>
            <FormDescription>This is the name of your AI.</FormDescription>
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
              <Textarea
                disabled={isLoading}
                placeholder="ex: An AI designed to help answer any questions about AppDirect's help center and knowledge base."
                rows={2}
                className="bg-background resize-none"
                {...field}
              />
            </FormControl>
            <FormDescription>
              Describe your AI in a brief statement. This is used to train your
              AI and generate the training material below.
            </FormDescription>
            <FormMessage />
          </FormItem>
        )}
      />
      <Button
        type="button"
        disabled={isLoading || generatingConversation || generatingInstruction}
        variant="outline"
        onClick={() => generateAll()}
      >
        {form.getValues("instructions") ? "Regenerate My AI" : "Create My AI"}
        {generatingConversation || generatingInstruction ? (
          <Loader className="w-4 h-4 ml-2 spinner" />
        ) : (
          <Wand2 className="w-4 h-4 ml-2" />
        )}
      </Button>
      {form.getValues("id") ||
      (form.getValues("instructions") && form.getValues("src")) ? (
        <>
          <div className="space-y-2 w-full">
            <div>
              <h3 className="text-lg font-medium">Character creation</h3>
              <p className="text-sm text-muted-foreground">
                Craft how you want your AI to perform and feel to users.
              </p>
            </div>
            <Separator className="bg-primary/10" />
          </div>
          <FormField
            name="src"
            render={({ field }) => (
              <FormItem className="flex flex-col items-center justify-center space-y-4">
                <FormControl>
                  <ImageUpload
                    disabled={isLoading}
                    onChange={field.onChange}
                    value={field.value}
                  />
                </FormControl>
                {advancedImage && (
                  <div>
                    <Textarea
                      disabled={isLoading || generatingImage}
                      rows={2}
                      placeholder="Image generation prompt"
                      className="bg-background resize-none mb-2"
                      value={imagePrompt}
                      onChange={(e) => setImagePrompt(e.target.value)}
                    />
                    <Select
                      disabled={isLoading || generatingImage}
                      onValueChange={(val) => setImageModel(val)}
                      value={imageModel}
                    >
                      <SelectTrigger className="bg-background">
                        <SelectValue placeholder="Select a model" />
                      </SelectTrigger>
                      <SelectContent>
                        {imageModels.map((model) => (
                          <SelectItem key={model.id} value={model.id}>
                            {model.name}
                          </SelectItem>
                        ))}
                      </SelectContent>
                    </Select>
                  </div>
                )}
                <div className="flex space-x-1">
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
                  {!advancedImage && (
                    <Button
                      type="button"
                      disabled={isLoading || generatingImage}
                      variant="ghost"
                      onClick={() => setAdvancedImage(true)}
                    >
                      <Settings className="w-4 h-4" />
                    </Button>
                  )}
                </div>

                <FormMessage />
              </FormItem>
            )}
          />
          <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
            <FormField
              name="introduction"
              control={form.control}
              render={({ field }) => (
                <FormItem className="col-span-2 md:col-span-1">
                  <FormLabel>Introduction</FormLabel>
                  <FormControl>
                    <Input
                      disabled={isLoading}
                      placeholder="ex: How may I be of assistance today?"
                      {...field}
                      value={field.value || ""}
                    />
                  </FormControl>
                  <FormDescription>
                    This is the first thing your AI will say.
                  </FormDescription>
                  <FormMessage />
                </FormItem>
              )}
            />
            {hasInstanceAccess &&
            <FormField
              control={form.control}
              name="categoryId"
              render={({ field }) => (
                <FormItem className="col-span-2 md:col-span-1">
                  <FormLabel>Category</FormLabel>
                  {false &&
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
                  }
                  <MultiSelect
                      itemLabel="Category Types"
                      items={CategoryTypes}
                      values={selectedValues}
                      onChange={(values) => {
                        setSelectedValues(values);
                        onSubmit({ CategoryTypes : values.map((category) => category.type) });
                      }}
                  />
                  <FormDescription>
                    Select the public category for your AI
                  </FormDescription>
                  <FormMessage />
                </FormItem>
              )}
            />
            }
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
                      <SelectItem key="PRIVATE" value="PRIVATE">
                        Restricted
                      </SelectItem>
                      <SelectItem key="ORGANIZATION" value="ORGANIZATION">
                        My Organization
                      </SelectItem>
                      <SelectItem
                        key="ANYONE_WITH_LINK"
                        value="ANYONE_WITH_LINK"
                      >
                        Anyone with the link
                      </SelectItem>
                    </SelectContent>
                  </Select>
                  {field.value === "PRIVATE" && (
                    <FormDescription>
                      Only you can access your AI
                    </FormDescription>
                  )}
                  {field.value === "ORGANIZATION" && (
                    <FormDescription>
                      Members of your organization can access your AI
                    </FormDescription>
                  )}
                  {field.value === "ANYONE_WITH_LINK" && (
                    <FormDescription>
                      Anyone with the link can access your AI
                    </FormDescription>
                  )}
                  <FormMessage />
                </FormItem>
              )}
            />
            <div className="mt-4 text-sm">
              {(form.watch("visibility") === AIVisibility.ORGANIZATION ||
                form.watch("visibility") === AIVisibility.ANYONE_WITH_LINK) && (
                <FormField
                  name="listInOrgCatalog"
                  control={form.control}
                  render={({ field }) => (
                    <FormItem>
                      <FormControl>
                        <Checkbox
                          id="listInOrgCatalog"
                          checked={field.value}
                          onCheckedChange={(val) => {
                            field.onChange(val);
                          }}
                        >
                          List in Organization Catalog
                        </Checkbox>
                      </FormControl>
                    </FormItem>
                  )}
                />
              )}
              {form.watch("visibility") === AIVisibility.ANYONE_WITH_LINK &&
                hasInstanceAccess && (
                  <FormField
                    name="listInPublicCatalog"
                    control={form.control}
                    render={({ field }) => (
                      <FormItem>
                        <Checkbox
                          id="listInPublicCatalog"
                          checked={field.value}
                          onCheckedChange={(val) => {
                            field.onChange(val);
                          }}
                        >
                          List in Public Catalog
                        </Checkbox>
                      </FormItem>
                    )}
                  />
                )}
            </div>
            {voiceEnabled && (
              <FormField
                name="talk"
                control={form.control}
                render={({ field }) =>
                  field.value && (
                    <FormItem>
                      <FormLabel>Voice</FormLabel>
                      <div className="flex">
                        <Select
                          disabled={isLoading}
                          value="en-US-JennyNeural"
                          defaultValue="en-US-JennyNeural"
                        >
                          <FormControl>
                            <SelectTrigger className="bg-background">
                              <SelectValue
                                defaultValue={field.value}
                                placeholder="Select a voice"
                              />
                            </SelectTrigger>
                          </FormControl>
                          <SelectContent>
                            {voices.map((model) => (
                              <SelectItem key={model.id} value={model.id}>
                                {model.name}
                              </SelectItem>
                            ))}
                          </SelectContent>
                        </Select>
                        <Button
                          type="button"
                          disabled={isLoading || generatingImage}
                          variant="ghost"
                          className="ml-2"
                          onClick={() => playTalk()}
                        >
                          <Play className="w-4 h-4" />
                        </Button>
                      </div>
                      <FormDescription>
                        Select a voice for your AI
                      </FormDescription>
                      <FormMessage />
                    </FormItem>
                  )
                }
              />
            )}
          </div>
          <div className="space-y-2 w-full">
            <div>
              <h3 className="text-lg font-medium">Training</h3>
              <p className="text-sm text-muted-foreground">
                Training information is used by your AI to understand its
                purpose. The more you refine this the more accurate your results
                will get, however to get started you can use the AI generation
                tools to test things out.
              </p>
            </div>
            <Separator className="bg-primary/10" />
          </div>

          <FormField
            name="instructions"
            control={form.control}
            render={({ field }) => (
              <FormItem>
                <FormLabel>Instructions for your AI</FormLabel>
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
                  This information will be used to determine the behavior of
                  your AI. You may give it instructions as you would an
                  employee. This helps teach the AI how it should behave and
                  what it should say. Provide specific and general instructions
                  that cover both what to say and how to talk to get a better
                  result.
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
                  Create an example conversation between a user and your AI.
                  This will teach the AI more about the type of response it
                  should provide to questions and better help it understand your
                  expectations.
                </FormDescription>
                <Button
                  type="button"
                  disabled={isLoading || generatingConversation}
                  variant="outline"
                  onClick={() => generateConversation()}
                >
                  Generate Conversation
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
        </>
      ) : null}

      <Dialog open={generatingAll}>
        <DialogContent noClose>
          <div className="flex justify-center items-center h-32">
            <Loader className="w-16 h-16 spinner" />
          </div>
          <div className="ml-4 text-center">{loadingMessage}</div>
        </DialogContent>
      </Dialog>

      <TalkModal />
    </div>
  );
};
