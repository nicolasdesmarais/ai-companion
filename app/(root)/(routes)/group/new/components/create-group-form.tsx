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
import { Input } from "@/components/ui/input";
import { Textarea } from "@/components/ui/textarea";
import { zodResolver } from "@hookform/resolvers/zod";
import { useForm } from "react-hook-form";
import * as z from "zod";

const groupFormSchema = z.object({
  name: z.string().min(1, {
    message: "Name is required.",
  }),
  accessLevel: z.union([z.literal("Everyone"), z.literal("Select teammates")]),
  teammates: z.string(),
});

export const CreateGroupForm = () => {
  const form = useForm<z.infer<typeof groupFormSchema>>({
    resolver: zodResolver(groupFormSchema),
    defaultValues: {
      name: "",
      accessLevel: "Everyone",
      teammates: "",
    },
  });

  const onSubmit = (values: z.infer<typeof groupFormSchema>) => {
    // Handle form submission logic here
    console.log(values);
  };

  return (
    <div className="h-full p-4 space-y-2 max-w-3xl mx-auto">
      <Form {...form}>
        <form
          onSubmit={form.handleSubmit(onSubmit)}
          className="space-y-8 pb-10"
        >
          <h3 className="text-lg font-medium">Create Group</h3>

          <FormField
            name="name"
            control={form.control}
            render={({ field }) => (
              <FormItem>
                <FormLabel>Name</FormLabel>
                <FormControl>
                  <Input placeholder="Group Name" {...field} />
                </FormControl>
                <FormMessage />
              </FormItem>
            )}
          />

          <div className="space-y-4">
            <FormLabel>Who can join?</FormLabel>
            <FormItem>
              <FormControl>
                <label>
                  <input
                    type="radio"
                    {...form.register("accessLevel")}
                    value="Everyone"
                  />
                  Everyone
                </label>
              </FormControl>
              <FormControl>
                <label>
                  <input
                    type="radio"
                    {...form.register("accessLevel")}
                    value="Select teammates"
                  />
                  Select teammates
                </label>
              </FormControl>
            </FormItem>
          </div>

          <FormField
            name="teammates"
            control={form.control}
            render={({ field }) => (
              <FormItem>
                <FormLabel>Add teammates</FormLabel>
                <FormControl>
                  <Textarea placeholder="Enter teammate names..." {...field} />
                </FormControl>
                <FormMessage />
              </FormItem>
            )}
          />

          <div className="w-full flex justify-center">
            <Button size="lg">Create Group</Button>
          </div>
        </form>
      </Form>
    </div>
  );
};
