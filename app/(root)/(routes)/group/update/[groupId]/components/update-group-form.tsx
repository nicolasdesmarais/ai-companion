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
import { GroupEntity } from "@/domain/entities/GroupEntity";
import { UpdateGroupRequest } from "@/domain/types/UpdateGroupRequest";
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

interface UpdateGroupFormProps {
  group: GroupEntity;
  groupUsers: { id: string; email: string }[];
}

export const UpdateGroupForm = ({
  group,
  groupUsers,
}: UpdateGroupFormProps) => {
  const router = useRouter();

  const initialTeammates = groupUsers.map((user) => user.email);

  const [selectedOption, setSelectedOption] =
    useState<GroupAvailability | null>(group.availability);
  const [currentTeammates, setCurrentTeammates] =
    useState<string[]>(initialTeammates);

  const form = useForm<z.infer<typeof groupFormSchema>>({
    resolver: zodResolver(groupFormSchema),
    defaultValues: {
      name: group.name,
      accessLevel: group.availability,
      teammates: "",
    },
  });

  const onSubmit = async (values: z.infer<typeof groupFormSchema>) => {
    const removedTeammates: string[] = groupUsers
      .filter(({ email }) => !currentTeammates.includes(email))
      .map(({ id }) => id);

    const request: UpdateGroupRequest = {
      name: values.name,
      availability: selectedOption || GroupAvailability.EVERYONE,
      memberEmailsToAdd: values.teammates,
      userIdsToRemove: removedTeammates,
    };

    await axios.put(`/api/v1/me/groups/${group.id}`, request);

    router.refresh();
    router.push("/");
  };

  const handleRemoveTeammate = (email: string) => {
    setCurrentTeammates((prevTeammates) =>
      prevTeammates.filter((t) => t !== email)
    );
  };

  return (
    <div className="h-full p-4 space-y-2 max-w-3xl mx-auto">
      <Form {...form}>
        <form
          onSubmit={form.handleSubmit(onSubmit)}
          className="space-y-8 pb-10"
        >
          <h3 className="text-lg font-medium">Update Group</h3>

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
            <>
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
              <h4 className="mt-4">
                Shared with {currentTeammates.length} people
              </h4>
              <ul className="list-disc pl-5 mt-2">
                {currentTeammates.map((teammate) => (
                  <li
                    key={teammate}
                    className="flex justify-between items-center mb-2"
                  >
                    {teammate}
                    <button
                      onClick={() => handleRemoveTeammate(teammate)}
                      className="text-red-600 px-2 py-1"
                    >
                      &#x2716;
                    </button>
                  </li>
                ))}
              </ul>
            </>
          )}

          <div className="w-full flex justify-center">
            <Button size="lg">Update</Button>
          </div>
        </form>
      </Form>
    </div>
  );
};
