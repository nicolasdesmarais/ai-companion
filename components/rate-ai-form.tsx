"use client";
import { Button } from "@/components/ui/button";
import {
  Form,
  FormControl,
  FormField,
  FormItem,
  FormLabel,
  FormMessage,
} from "@/components/ui/form";
import { Textarea } from "@/components/ui/textarea";
import { zodResolver } from "@hookform/resolvers/zod";
import { AI } from "@prisma/client";
import axios from "axios";
import { useForm } from "react-hook-form";
import * as z from "zod";
import { useToast } from "./ui/use-toast";
import { StarRating } from "./star-rating";

const rateFormSchema = z.object({
  rating: z.number().min(1).max(5),
  review: z.string().optional(),
});

interface RateAIFormProps {
  ai: AI;
  onSuccess: () => void;
}

export const RateAIForm = ({ ai, onSuccess }: RateAIFormProps) => {
  const { toast } = useToast();

  const form = useForm<z.infer<typeof rateFormSchema>>({
    resolver: zodResolver(rateFormSchema),
    defaultValues: {
      rating: 0,
      review: "",
    },
  });

  const onSubmit = async (values: z.infer<typeof rateFormSchema>) => {
    try {
      const response = await axios.put(`/api/v1/ai/${ai.id}/rate`, values);
      if (response.status === 200) {
        onSuccess();
      } else {
        toast({
          description: `Something went wrong (${response.status})`,
          variant: "destructive",
        });
      }
    } catch (error) {
      toast({
        description: "Something went wrong",
        variant: "destructive",
      });
    }
  };

  return (
    <div className="h-full p-4 space-y-2 max-w-3xl">
      <Form {...form}>
        <form onSubmit={form.handleSubmit(onSubmit)} className="space-y-8">
          <h3 className="text-lg font-medium">Review AI - {ai.name}</h3>

          <FormField
            name="rating"
            control={form.control}
            render={({ field }) => (
              <FormItem>
                <FormLabel>Your Rating</FormLabel>
                <FormControl>
                  <div className="flex items-center">
                    <StarRating size="large" {...field} />
                  </div>
                </FormControl>
                <FormMessage />
              </FormItem>
            )}
          />
          <FormField
            name="review"
            control={form.control}
            render={({ field }) => (
              <FormItem>
                <FormLabel>Your Review</FormLabel>
                <FormControl>
                  <Textarea
                    placeholder="This AI changed my life..."
                    {...field}
                  />
                </FormControl>
                <FormMessage />
              </FormItem>
            )}
          />
          <div className="w-full flex flex-row-reverse">
            <Button size="lg" variant="ring">
              Submit
            </Button>
          </div>
        </form>
      </Form>
    </div>
  );
};
