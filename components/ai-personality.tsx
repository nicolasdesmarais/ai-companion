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
interface SelectDataSourceProps {
  initialAi: ExtendedAI | null;
  form: any;
  aiModels: AIModel[];
}

export const AIPersonality = ({
  initialAi,
  form,
  aiModels,
}: SelectDataSourceProps) => {
  const isLoading = form.formState.isSubmitting;
  const modelId = form.getValues("modelId");
  const model = aiModels.find((model) => model.id === modelId);

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
      {model.options.topP && (
        <FormField
          name="options.topP"
          control={form.control}
          render={({ field }) => (
            <FormItem className="col-span-2 md:col-span-1">
              <FormLabel>
                Nucleus Sampling Factor (top_p):{" "}
                {field.value || model.options.topP.default}
              </FormLabel>
              <FormControl>
                <Slider
                  {...field}
                  disabled={isLoading}
                  max={model.options.topP.max}
                  min={model.options.topP.min}
                  step={model.options.topP.step}
                  defaultValue={[model.options.topP.default]}
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
      {model.options.maxTokens && (
        <FormField
          name="options.maxTokens"
          control={form.control}
          render={({ field }) => (
            <FormItem className="col-span-2 md:col-span-1">
              <FormLabel>
                Max Tokens: {field.value || model.options.maxTokens.default}
              </FormLabel>
              <FormControl>
                <Slider
                  {...field}
                  disabled={isLoading}
                  max={model.options.maxTokens.max}
                  min={model.options.maxTokens.min}
                  step={model.options.maxTokens.step}
                  defaultValue={[model.options.maxTokens.default]}
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
      {model.options.frequencyPenalty && (
        <FormField
          name="options.frequencyPenalty"
          control={form.control}
          render={({ field }) => (
            <FormItem className="col-span-2 md:col-span-1">
              <FormLabel>
                Frequency Penalty:{" "}
                {field.value || model.options.frequencyPenalty?.default}
              </FormLabel>
              <FormControl>
                <Slider
                  {...field}
                  disabled={isLoading}
                  max={model.options.frequencyPenalty?.max}
                  min={model.options.frequencyPenalty?.min}
                  step={model.options.frequencyPenalty?.step}
                  defaultValue={[model.options.frequencyPenalty?.default || 0]}
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
      {model.options.presencePenalty && (
        <FormField
          name="options.presencePenalty"
          control={form.control}
          render={({ field }) => (
            <FormItem className="col-span-2 md:col-span-1">
              <FormLabel>
                Presence Penalty:{" "}
                {field.value || model.options.presencePenalty?.default}
              </FormLabel>
              <FormControl>
                <Slider
                  {...field}
                  disabled={isLoading}
                  max={model.options.presencePenalty?.max}
                  min={model.options.presencePenalty?.min}
                  step={model.options.presencePenalty?.step}
                  defaultValue={[model.options.presencePenalty?.default || 0]}
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
    </div>
  );
};
