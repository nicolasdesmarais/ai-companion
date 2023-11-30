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
import { Slider } from "@/components/ui/slider";
import { AIModel } from "@/src/domain/models/AIModel";
import { Prisma } from "@prisma/client";
import { Input } from "./ui/input";
import { Textarea } from "./ui/textarea";
import { Checkbox } from "./ui/checkbox";

const SAMPLE_DESCRIPTION = `The AI you've engaged is engineered with advanced capabilities, designed to address and respond to an array of questions you might have, regardless of their complexity. It is backed by a rich and extensive compilation of documents that have meticulously uploaded, providing a broad and deep knowledge base to draw from. This AI, with its vast knowledge, stands ready to offer insightful answers to your diverse queries, whether you're seeking simple clarifications or deep, complex explorations. Its singular aim is to ensure you receive the precise information you need, with speed and accuracy, thereby streamlining your decision-making process and enhancing your productivity.`;

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
interface ProfileSourceProps {
  initialAi: ExtendedAI | null;
  form: any;
  aiModels: AIModel[];
}

export const AIProfile = ({
  initialAi,
  form,
  aiModels,
}: ProfileSourceProps) => {
  const isLoading = form.formState.isSubmitting;

  return (
    <div className="h-full p-4 space-y-2 max-w-3xl mx-auto">
      <div className="space-y-2 w-full col-span-2">
        <div>
          <h3 className="text-lg font-medium">Profile Information</h3>
          <p className="text-sm text-muted-foreground">
            Text and images to display in the catalog of AIs.
          </p>
        </div>
        <Separator className="bg-primary/10" />
      </div>
      <div className="pt-2 space-y-4">
        <FormField
          name="headline"
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
          name="profileDescription"
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
      </div>
      <div className="space-y-2 w-full col-span-2 pt-4">
        <div>
          <h3 className="text-lg font-medium">
            Training Specifications Visibility
          </h3>
          <p className="text-sm text-muted-foreground">
            Decide what information you trained the AI on is visible to the
            users
          </p>
        </div>
        <Separator className="bg-primary/10" />
        <div className="pt-2 space-y-8">
          <FormField
            name="showCharacter"
            control={form.control}
            render={({ field }) => (
              <FormItem className="col-span-2 md:col-span-1">
                <FormControl>
                  <Checkbox>
                    <div>
                      Show Character Information
                      <FormDescription>
                        Checking this box will make the Character instructions
                        and example conversations visible on the AI profile.
                      </FormDescription>
                    </div>
                  </Checkbox>
                </FormControl>
                <FormMessage />
              </FormItem>
            )}
          />
          <FormField
            name="showTraining"
            control={form.control}
            render={({ field }) => (
              <FormItem className="col-span-2 md:col-span-1">
                <FormControl>
                  <Checkbox>
                    <div>
                      Show Training Materials
                      <FormDescription>
                        Checking this box will make the names of all websites
                        and files visible on the AI profile.
                      </FormDescription>
                    </div>
                  </Checkbox>
                </FormControl>
                <FormMessage />
              </FormItem>
            )}
          />
          <FormField
            name="showCharacter"
            control={form.control}
            render={({ field }) => (
              <FormItem className="col-span-2 md:col-span-1">
                <FormControl>
                  <Checkbox>
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
