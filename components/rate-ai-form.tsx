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
import { AIDetailDto } from "@/src/domain/models/AIApi";
import { zodResolver } from "@hookform/resolvers/zod";
import axios from "axios";
import { Loader } from "lucide-react";
import { useEffect, useState } from "react";
import { useForm } from "react-hook-form";
import * as z from "zod";
import { StarRating } from "./star-rating";
import { Input } from "./ui/input";
import { useToast } from "./ui/use-toast";

const rateFormSchema = z.object({
  rating: z.number().min(1).max(5),
  headline: z.string().optional(),
  review: z.string().optional(),
});

interface RateAIFormProps {
  ai: AIDetailDto;
  onSuccess: () => void;
}

export const RateAIForm = ({ ai, onSuccess }: RateAIFormProps) => {
  const { toast } = useToast();
  const [isLoading, setIsLoading] = useState(true);

  const form = useForm<z.infer<typeof rateFormSchema>>({
    resolver: zodResolver(rateFormSchema),
    defaultValues: {
      rating: 0,
      review: "",
    },
  });

  useEffect(() => {
    const fetchRating = async () => {
      setIsLoading(true);
      const response = await axios.get(`/api/v1/ai/${ai.id}/rating`);
      const { rating, review } = response.data[0] || {};
      form.setValue("rating", rating);
      form.setValue("review", review);
      setIsLoading(false);
    };
    fetchRating();
  }, [ai.id, form]);

  const onSubmit = async (values: z.infer<typeof rateFormSchema>) => {
    try {
      const response = await axios.put(`/api/v1/ai/${ai.id}/rating`, values);
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
          {isLoading ? (
            <div className="flex justify-center items-center h-32">
              <Loader className="w-16 h-16 spinner" />
            </div>
          ) : (
            <>
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
                name="headline"
                control={form.control}
                render={({ field }) => (
                  <FormItem>
                    <FormLabel>Headline</FormLabel>
                    <FormControl>
                      <Input placeholder="Great AI" {...field} />
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
            </>
          )}
        </form>
      </Form>
    </div>
  );
};
