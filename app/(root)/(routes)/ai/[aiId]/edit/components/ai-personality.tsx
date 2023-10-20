"use client";
import { Prisma } from "@prisma/client";
import {
  FormControl,
  FormDescription,
  FormField,
  FormItem,
  FormLabel,
  FormMessage,
} from "@/components/ui/form";
import { Button } from "@/components/ui/button";
import { Separator } from "@/components/ui/separator";
import { Wand2 } from "lucide-react";
import { Slider } from "@/components/ui/slider";
import { models } from "./ai-models";

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
interface SelectDataSourceProps {
  initialAi: ExtendedCompanion | null;
  form: any;
}

export const AIPersonality = ({ initialAi, form }: SelectDataSourceProps) => {
  const isLoading = form.formState.isSubmitting;
  const modelId = form.getValues("modelId");
  const model = models.find((model) => model.id === modelId);

  if (!model) {
    return null;
  }

  return (
    <div className="h-full p-4 space-y-2 max-w-3xl mx-auto">
      <div className="space-y-2 w-full col-span-2">
        <div>
          <h3 className="text-lg font-medium">AI Model Parameters</h3>
          <p className="text-sm text-muted-foreground">
            Adjust the probability distribution of the predicted output.
          </p>
        </div>
        <Separator className="bg-primary/10" />
      </div>

      {model.options.temperature && (
        <FormField
          name="options.temperature"
          control={form.control}
          render={({ field }) => (
            <FormItem className="col-span-2 md:col-span-1">
              <FormLabel>
                Temperature: {field.value || model.options.temperature.default}
              </FormLabel>
              <FormControl>
                <Slider
                  {...field}
                  disabled={isLoading}
                  max={model.options.temperature.max}
                  min={model.options.temperature.min}
                  step={model.options.temperature.step}
                  defaultValue={[model.options.temperature.default]}
                />
              </FormControl>
              <FormDescription>
                The temperature is used to control the randomness of the output.
                When you set it higher, you&apos;ll get more random outputs.
              </FormDescription>
              <FormMessage />
            </FormItem>
          )}
        />
      )}
      {model.options.top_p && (
        <FormField
          name="options.top_p"
          control={form.control}
          render={({ field }) => (
            <FormItem className="col-span-2 md:col-span-1">
              <FormLabel>
                Top_P: {field.value || model.options.top_p.default}
              </FormLabel>
              <FormControl>
                <Slider
                  {...field}
                  disabled={isLoading}
                  max={model.options.top_p.max}
                  min={model.options.top_p.min}
                  step={model.options.top_p.step}
                  defaultValue={[model.options.top_p.default]}
                />
              </FormControl>
              <FormDescription>
                The Top_P parameter controls how many of the highest-probability
                words are selected to be included in the generated text.
                Specifically, it sets a threshold such that only the words with
                probabilities greater than or equal to the threshold will be
                included.
              </FormDescription>
              <FormMessage />
            </FormItem>
          )}
        />
      )}
      {model.options.max_tokens && (
        <FormField
          name="options.max_tokens"
          control={form.control}
          render={({ field }) => (
            <FormItem className="col-span-2 md:col-span-1">
              <FormLabel>
                Max Tokens: {field.value || model.options.max_tokens.default}
              </FormLabel>
              <FormControl>
                <Slider
                  {...field}
                  disabled={isLoading}
                  max={model.options.max_tokens.max}
                  min={model.options.max_tokens.min}
                  step={model.options.max_tokens.step}
                  defaultValue={[model.options.max_tokens.default]}
                />
              </FormControl>
              <FormDescription>
                The maximum number of tokens to generate in the chat completion.
                The total length of input tokens and generated tokens is limited
                by the model&apos;s context length.
              </FormDescription>
              <FormMessage />
            </FormItem>
          )}
        />
      )}
      {model.options.frequency_penalty && (
        <FormField
          name="options.frequency_penalty"
          control={form.control}
          render={({ field }) => (
            <FormItem className="col-span-2 md:col-span-1">
              <FormLabel>
                Frequency Penalty:{" "}
                {field.value || model.options.frequency_penalty?.default}
              </FormLabel>
              <FormControl>
                <Slider
                  {...field}
                  disabled={isLoading}
                  max={model.options.frequency_penalty?.max}
                  min={model.options.frequency_penalty?.min}
                  step={model.options.frequency_penalty?.step}
                  defaultValue={[model.options.frequency_penalty?.default || 0]}
                />
              </FormControl>
              <FormDescription>
                Positive values penalize new tokens based on their existing
                frequency in the text so far, decreasing the model&apos;s
                likelihood to repeat the same line verbatim.
              </FormDescription>
              <FormMessage />
            </FormItem>
          )}
        />
      )}
      {model.options.presence_penalty && (
        <FormField
          name="options.presence_penalty"
          control={form.control}
          render={({ field }) => (
            <FormItem className="col-span-2 md:col-span-1">
              <FormLabel>
                Presence Penalty:{" "}
                {field.value || model.options.presence_penalty?.default}
              </FormLabel>
              <FormControl>
                <Slider
                  {...field}
                  disabled={isLoading}
                  max={model.options.presence_penalty?.max}
                  min={model.options.presence_penalty?.min}
                  step={model.options.presence_penalty?.step}
                  defaultValue={[model.options.presence_penalty?.default || 0]}
                />
              </FormControl>
              <FormDescription>
                Positive values penalize new tokens based on whether they appear
                in the text so far, increasing the model&apos;s likelihood to
                talk about new topics.
              </FormDescription>
              <FormMessage />
            </FormItem>
          )}
        />
      )}
      <div className="w-full flex justify-between">
        <Button size="lg" disabled={isLoading}>
          {initialAi ? "Save your AI" : "Create your AI"}
          <Wand2 className="w-4 h-4 ml-2" />
        </Button>
      </div>
    </div>
  );
};
