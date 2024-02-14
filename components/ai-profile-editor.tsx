"use client";
import {
  FormControl,
  FormDescription,
  FormField,
  FormItem,
  FormLabel,
  FormMessage,
} from "@/components/ui/form";
import { Separator } from "@/components/ui/separator";
import { AIDetailDto } from "@/src/domain/models/AI";
import { ChatMessageDto } from "@/src/domain/models/Chats";
import { Loader, Plus, Trash, Wand2 } from "lucide-react";
import { useState } from "react";
import { Drawer } from "./drawer";
import { ExampleChat } from "./example-chat";
import { TestChat } from "./test-chat";
import { Button } from "./ui/button";
import { Checkbox } from "./ui/checkbox";
import { Input } from "./ui/input";
import { Textarea } from "./ui/textarea";

const SAMPLE_DESCRIPTION = `The AI you've engaged is engineered with advanced capabilities, designed to address and respond to an array of questions you might have, regardless of their complexity. It is backed by a rich and extensive compilation of documents that have meticulously uploaded, providing a broad and deep knowledge base to draw from. This AI, with its vast knowledge, stands ready to offer insightful answers to your diverse queries, whether you're seeking simple clarifications or deep, complex explorations. Its singular aim is to ensure you receive the precise information you need, with speed and accuracy, thereby streamlining your decision-making process and enhancing your productivity.`;
const SAMPLE_TRAINING = `This AI is trained on a variety of sources, including websites, documents, and files. It is also trained on a variety of websites, including Wikipedia, Google, and StackOverflow.`;

interface ProfileSourceProps {
  ai: AIDetailDto | null;
  form: any;
}

export const AIProfileEditor = ({ ai, form }: ProfileSourceProps) => {
  const [messages, setMessages] = useState<ChatMessageDto[]>([]);
  const [chatOpen, setChatOpen] = useState(false);
  const [generatingProfile, setGeneratingProfile] = useState(false);
  if (!ai) {
    return null;
  }
  const isLoading = form.formState.isSubmitting;

  const generateProfile = async () => {
    setGeneratingProfile(true);
    const response = await fetch(`/api/v1/ai/${ai.id}/generate-profile`, {
      method: "PUT",
    });
    if (response.ok) {
      const data = await response.json();
      const currentProfile = form.getValues("profile");
      for (let i = 0; i < data.features.length; i++) {
        form.setValue(`profile.features[${i}].title`, data.features[i].title, {
          shouldDirty: true,
        });
        form.setValue(
          `profile.features[${i}].description`,
          data.features[i].description,
          {
            shouldDirty: true,
          }
        );
      }
      form.setValue(
        "profile",
        { ...currentProfile, ...data },
        {
          shouldDirty: true,
        }
      );
    }
    setGeneratingProfile(false);
  };

  return (
    <div className="h-full p-4 space-y-2 max-w-3xl mx-auto">
      <div className="space-y-2 w-full col-span-2">
        <div className="relative">
          <h3 className="text-lg font-medium">Profile Information</h3>
          <p className="text-sm text-muted-foreground">
            Text and images to display in the catalog of AIs.
          </p>
          <Button
            className="mt-2 md:absolute top-0 right-0"
            type="button"
            disabled={isLoading || generatingProfile}
            variant="outline"
            onClick={() => generateProfile()}
          >
            Generate Profile
            {generatingProfile ? (
              <Loader className="w-4 h-4 ml-2 spinner" />
            ) : (
              <Wand2 className="w-4 h-4 ml-2" />
            )}
          </Button>
        </div>
        <Separator className="bg-primary/10" />
      </div>
      <div className="pt-2 space-y-4">
        <FormField
          name="profile.headline"
          control={form.control}
          render={({ field }) => (
            <FormItem className="col-span-2 md:col-span-1">
              <FormLabel>Headline</FormLabel>
              <FormDescription>
                One sentence or short paragraph to describe your AI for users.
              </FormDescription>
              <FormControl>
                <Input
                  disabled={isLoading}
                  placeholder="The ultimate solution to all your AI needs."
                  {...field}
                />
              </FormControl>
              <FormMessage />
            </FormItem>
          )}
        />
        <FormField
          name="profile.description"
          control={form.control}
          render={({ field }) => (
            <FormItem>
              <FormLabel>Description</FormLabel>
              <FormDescription>
                One paragraph or brief summary of the purpose and capabilities
                of your AI.
              </FormDescription>
              <FormControl>
                <Textarea
                  disabled={isLoading}
                  rows={9}
                  className="bg-background resize-none"
                  placeholder={SAMPLE_DESCRIPTION}
                  {...field}
                />
              </FormControl>
              <FormMessage />
            </FormItem>
          )}
        />
        <FormField
          name="profile.features"
          control={form.control}
          render={({ field }) => (
            <FormItem>
              <FormLabel>Features</FormLabel>
              <FormDescription>
                Write a few features of your AI to help explain its value to
                users.
              </FormDescription>
              <div className="border-l border-ring pl-4 mt-4">
                {(field.value || [{}]).map((feature: any, index: number) => (
                  <div key={index}>
                    <div className="flex items-center">
                      <FormLabel className="mr-2">
                        Feature {index + 1}
                      </FormLabel>
                      <Button
                        disabled={isLoading}
                        onClick={() => {
                          field.onChange([
                            ...field.value.slice(0, index),
                            ...field.value.slice(index + 1),
                          ]);
                        }}
                        size="icon"
                        variant="ghost"
                        type="button"
                      >
                        <Trash className="w-4 h-4" />
                      </Button>
                    </div>
                    <div className="border-l border-ring pl-4 pb-2 mt-2 mb-4 space-y-2">
                      <FormField
                        name={`profile.features[${index}].title`}
                        control={form.control}
                        render={({ field }) => (
                          <div>
                            <FormLabel>Feature Title</FormLabel>
                            <FormControl>
                              <Input
                                disabled={isLoading}
                                placeholder="Very Smart"
                                className="mt-2"
                                {...field}
                              />
                            </FormControl>
                            <FormMessage />
                          </div>
                        )}
                      />
                      <FormField
                        name={`profile.features[${index}].description`}
                        control={form.control}
                        render={({ field }) => (
                          <div>
                            <FormLabel>Feature Short Description</FormLabel>
                            <FormControl>
                              <Input
                                disabled={isLoading}
                                placeholder="This AI is very intelligent and able to understand complete sentences."
                                className="mt-2"
                                {...field}
                              />
                            </FormControl>
                            <FormMessage />
                          </div>
                        )}
                      />
                    </div>
                  </div>
                ))}
                <Button
                  onClick={() => {
                    field.onChange([...(field.value || [{}]), {}]);
                  }}
                  disabled={isLoading}
                  variant="ring"
                  type="button"
                >
                  Add Another Feature
                </Button>
              </div>
            </FormItem>
          )}
        />
        <FormField
          name="profile.conversations"
          control={form.control}
          render={({ field }) => (
            <FormItem>
              <FormLabel>Example Conversations</FormLabel>
              <FormDescription>
                Provide example question and answer so that users can understand
                the type of conversations your AI is good at.
              </FormDescription>
              <div className="flex flex-wrap">
                <div
                  key={`example-conversation-add`}
                  className="mt-4 w-1/3 h-64 p-2 justify-center flex items-center"
                >
                  <Drawer
                    trigger={
                      <div className="cursor-pointer hover:bg-primary/10 rounded-md text-muted-foreground">
                        <Plus className="w-24 h-24" />
                      </div>
                    }
                    open={chatOpen}
                    setOpen={setChatOpen}
                  >
                    <TestChat
                      ai={ai}
                      messages={messages}
                      setMessages={setMessages}
                      actions={
                        <div className="flex justify-between">
                          <Button
                            onClick={() => {
                              setMessages([]);
                            }}
                            disabled={isLoading}
                            type="button"
                          >
                            Restart Chat
                          </Button>
                          <Button
                            onClick={() => {
                              const currentConversations = field.value || [];
                              field.onChange([
                                { messages },
                                ...currentConversations,
                              ]);
                              setMessages([]);
                              setChatOpen(false);
                            }}
                            disabled={isLoading}
                            variant="ring"
                            type="button"
                          >
                            Save Example Chat
                          </Button>
                        </div>
                      }
                    />
                  </Drawer>
                </div>
                {field.value?.map((conversation: any, index: number) => (
                  <div
                    key={`example-conversation-${index}`}
                    className="mt-4 w-1/3 h-64 p-2 relative"
                  >
                    <Button
                      className="absolute bottom-4 right-4 z-10"
                      disabled={isLoading}
                      onClick={() => {
                        field.onChange([
                          ...field.value.slice(0, index),
                          ...field.value.slice(index + 1),
                        ]);
                      }}
                      variant="outline"
                      size="icon"
                      type="button"
                    >
                      <Trash className="w-4 h-4" />
                    </Button>
                    <ExampleChat messages={conversation.messages} ai={ai} />
                  </div>
                ))}
              </div>
            </FormItem>
          )}
        />
      </div>
      <div className="space-y-2 w-full col-span-2 pt-4">
        <h3 className="text-lg font-medium">
          Training Specifications Visibility
        </h3>
        <p className="text-sm text-muted-foreground">
          Decide what information you trained the AI on is visible to the users
        </p>
        <Separator className="bg-primary/10" />
        <div className="pt-2">
          <div className="pb-8">
            <FormField
              name="profile.trainingDescription"
              control={form.control}
              render={({ field }) => (
                <FormItem>
                  <FormLabel>Training Description</FormLabel>
                  <FormDescription>
                    One paragraph or brief summary explaining what information
                    this AI is trained on.
                  </FormDescription>
                  <FormControl>
                    <Textarea
                      disabled={isLoading}
                      rows={6}
                      className="bg-background resize-none"
                      placeholder={SAMPLE_TRAINING}
                      {...field}
                    />
                  </FormControl>
                  <FormMessage />
                </FormItem>
              )}
            />
          </div>
          <FormField
            name="profile.showCharacter"
            control={form.control}
            render={({ field }) => (
              <FormItem>
                <FormControl>
                  <Checkbox
                    checked={field.value}
                    onCheckedChange={(val) => {
                      field.onChange(val);
                    }}
                    {...field}
                  >
                    <div>
                      Show Character Information
                      <div>
                        <FormDescription>
                          Checking this box will make the Character instructions
                          and example conversations visible on the AI profile.
                        </FormDescription>
                      </div>
                    </div>
                  </Checkbox>
                </FormControl>
                <FormMessage />
              </FormItem>
            )}
          />
          <FormField
            name="profile.showTraining"
            control={form.control}
            render={({ field }) => (
              <FormItem>
                <FormControl>
                  <Checkbox
                    checked={field.value}
                    onCheckedChange={(val) => {
                      field.onChange(val);
                    }}
                    {...field}
                  >
                    <div>
                      Show Training Materials
                      <div>
                        <FormDescription>
                          Checking this box will make the names of all websites
                          and files visible on the AI profile.
                        </FormDescription>
                      </div>
                    </div>
                  </Checkbox>
                </FormControl>
                <FormMessage />
              </FormItem>
            )}
          />
          <FormField
            name="profile.showPersonality"
            control={form.control}
            render={({ field }) => (
              <FormItem>
                <FormControl>
                  <Checkbox
                    checked={field.value}
                    onCheckedChange={(val) => {
                      field.onChange(val);
                    }}
                    {...field}
                  >
                    <div>
                      Show Personality Settings
                      <FormDescription>
                        Checking this box will make the personality settings
                        visible on the AI profile.
                      </FormDescription>
                    </div>
                  </Checkbox>
                </FormControl>
                <FormMessage />
              </FormItem>
            )}
          />
        </div>
      </div>
    </div>
  );
};
