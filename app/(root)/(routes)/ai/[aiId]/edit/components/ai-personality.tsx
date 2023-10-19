"use client";
import * as z from "zod";
import { Prisma } from "@prisma/client";
import {
  Form,
  FormControl,
  FormDescription,
  FormField,
  FormItem,
  FormLabel,
  FormMessage,
} from "@/components/ui/form";
import { useForm } from "react-hook-form";
import { zodResolver } from "@hookform/resolvers/zod";
import { Button } from "@/components/ui/button";
import { Separator } from "@/components/ui/separator";
import { useToast } from "@/components/ui/use-toast";
import { Wand2 } from "lucide-react";
import { Input } from "@/components/ui/input";

const formSchema = z.object({
  temperature: z.string(),
  top_p: z.string(),
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
interface SelectDataSourceProps {
  initialAi: ExtendedCompanion | null;
}

export const AIPersonality = ({ initialAi }: SelectDataSourceProps) => {
  const { toast } = useToast();
  const form = useForm<z.infer<typeof formSchema>>({
    resolver: zodResolver(formSchema),
    defaultValues: {
      temperature: "",
      top_p: "",
    },
  });
  const isLoading = form.formState.isSubmitting;
  const onSubmit = async (values: z.infer<typeof formSchema>) => {
    console.log("todo: implement");
  };
  return (
    <div className="h-full p-4 space-y-2 max-w-3xl mx-auto">
      <Form {...form}>
        <form
          onSubmit={form.handleSubmit(onSubmit)}
          className="space-y-8 pb-10"
        >
          <div className="space-y-2 w-full col-span-2">
            <div>
              <h3 className="text-lg font-medium">AI Model Parameters</h3>
              <p className="text-sm text-muted-foreground">
                Adjust the probability distribution of the predicted output.
              </p>
            </div>
            <Separator className="bg-primary/10" />
          </div>
          <FormField
            name="temperature"
            control={form.control}
            render={({ field }) => (
              <FormItem className="col-span-2 md:col-span-1">
                <FormLabel>Temperature</FormLabel>
                <FormControl>
                  <Input disabled={isLoading} {...field} />
                </FormControl>
                <FormDescription>
                  The temperature is used to control the randomness of the
                  output. When you set it higher, you&apos;ll get more random
                  outputs.
                </FormDescription>
                <FormMessage />
              </FormItem>
            )}
          />
          <FormField
            name="top_p"
            control={form.control}
            render={({ field }) => (
              <FormItem className="col-span-2 md:col-span-1">
                <FormLabel>Top_p</FormLabel>
                <FormControl>
                  <Input disabled={isLoading} {...field} />
                </FormControl>
                <FormDescription>
                  The Top_P parameter controls how many of the
                  highest-probability words are selected to be included in the
                  generated text. Specifically, it sets a threshold such that
                  only the words with probabilities greater than or equal to the
                  threshold will be included.
                </FormDescription>
                <FormMessage />
              </FormItem>
            )}
          />
          <div className="w-full flex justify-between">
            <Button size="lg" disabled={isLoading}>
              {initialAi ? "Save your AI" : "Create your AI"}
              <Wand2 className="w-4 h-4 ml-2" />
            </Button>
          </div>
        </form>
      </Form>
    </div>
  );
};
