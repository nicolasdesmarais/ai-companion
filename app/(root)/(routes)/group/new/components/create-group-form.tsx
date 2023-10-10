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
import { CreateGroupRequest } from "@/domain/types/CreateGroupRequest";
import { zodResolver } from "@hookform/resolvers/zod";
import { GroupAvailability } from "@prisma/client";
import axios from "axios";
import { useRouter } from "next/navigation";
import { useState } from "react";
import { useForm } from "react-hook-form";
import * as z from "zod";

const groupFormSchema = z.object({
  name: z.string().min(1, {
    message: "Name is required.",
  }),
  accessLevel: z.enum([
    GroupAvailability.EVERYONE,
    GroupAvailability.RESTRICTED,
  ]),
  teammates: z.string(),
});

export const CreateGroupForm = () => {
  const router = useRouter();
  const [selectedOption, setSelectedOption] =
    useState<GroupAvailability | null>(GroupAvailability.EVERYONE);

  const form = useForm<z.infer<typeof groupFormSchema>>({
    resolver: zodResolver(groupFormSchema),
    defaultValues: {
      name: "",
      accessLevel: GroupAvailability.EVERYONE,
      teammates: "",
    },
  });

  const onSubmit = async (values: z.infer<typeof groupFormSchema>) => {
    const request: CreateGroupRequest = {
      name: values.name,
      availability: selectedOption || GroupAvailability.EVERYONE,
      memberEmails: values.teammates,
    };

    await axios.post(`/api/v1/me/groups`, request);

    router.refresh();
    router.push("/");
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
                <div>
                  <label>
                    <input
                      type="radio"
                      value={GroupAvailability.EVERYONE}
                      checked={selectedOption === GroupAvailability.EVERYONE}
                      onChange={(e) =>
                        setSelectedOption(e.target.value as GroupAvailability)
                      }
                    />
                    Everyone in your company
                  </label>
                </div>
              </FormControl>
              <FormControl>
                <div>
                  <label>
                    <input
                      type="radio"
                      value={GroupAvailability.RESTRICTED}
                      checked={selectedOption === GroupAvailability.RESTRICTED}
                      onChange={(e) =>
                        setSelectedOption(e.target.value as GroupAvailability)
                      }
                    />
                    Select Team Members
                  </label>
                </div>
              </FormControl>
            </FormItem>
          </div>

          {selectedOption === GroupAvailability.RESTRICTED && (
            <FormField
              name="teammates"
              control={form.control}
              render={({ field }) => (
                <FormItem>
                  <FormLabel>Add teammates</FormLabel>
                  <FormControl>
                    <Textarea
                      placeholder="Ex: jennifer.wallace@acme.com, joe.hamm@acme.com"
                      {...field}
                    />
                  </FormControl>
                  <FormMessage />
                </FormItem>
              )}
            />
          )}

          <div className="w-full flex justify-center">
            <Button size="lg">Create</Button>
          </div>
        </form>
      </Form>
    </div>
  );
};
