"use client";

import { useEffect, useState } from "react";
import { useForm } from "react-hook-form";
import { zodResolver } from "@hookform/resolvers/zod";
import axios from "axios";

import {
  Dialog,
  DialogContent,
  DialogDescription,
  DialogFooter,
  DialogHeader,
  DialogTitle,
} from "@/components/ui/dialog";
import {
  Form,
  FormControl,
  FormField,
  FormItem,
  FormLabel,
  FormMessage,
} from "@/components/ui/form";
import { Input } from "@/components/ui/input";
import { useGroupModal } from "@/hooks/use-group-modal";
import { Button } from "@/components/ui/button";
import { Separator } from "@/components/ui/separator";
import { useToast } from "@/components/ui/use-toast";
import { GroupAvailability } from "@prisma/client";
import { Textarea } from "@/components/ui/textarea";
import { CreateGroupRequest } from "@/domain/types/CreateGroupRequest";
import { Loader } from "lucide-react";
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

export const GroupModal = () => {
  const groupModal = useGroupModal();
  const [isMounted, setIsMounted] = useState(false);
  const [loading, setLoading] = useState(false);
  const [selectedOption, setSelectedOption] =
    useState<GroupAvailability | null>(GroupAvailability.EVERYONE);
  const { toast } = useToast();

  const form = useForm<z.infer<typeof groupFormSchema>>({
    resolver: zodResolver(groupFormSchema),
    defaultValues: {
      name: "",
      accessLevel: GroupAvailability.EVERYONE,
      teammates: "",
    },
  });

  useEffect(() => {
    setIsMounted(true);
  }, []);

  const onSubmit = async (values: z.infer<typeof groupFormSchema>) => {
    try {
      setLoading(true);
      const request: CreateGroupRequest = {
        name: values.name,
        availability: selectedOption || GroupAvailability.EVERYONE,
        memberEmails: values.teammates,
      };

      const response = await axios.post(`/api/v1/me/groups`, request);
      if (response.status === 200) {
        toast({
          description: "Group created successfully",
        });
        groupModal.onUpdate(response.data);
        form.reset();
      }
    } catch (error) {
      toast({
        description: "Something went wrong",
        variant: "destructive",
      });
    } finally {
      setLoading(false);
    }
  };

  if (!isMounted) {
    return null;
  }

  return (
    <Dialog open={groupModal.isOpen} onOpenChange={groupModal.onClose}>
      <DialogContent>
        <DialogHeader className="space-y-4">
          <DialogTitle className="text-center">Create a Group</DialogTitle>
          <DialogDescription className="text-center space-y-2">
            Create a group to share your AI with select people.
          </DialogDescription>
        </DialogHeader>
        <Separator />
        <Form {...form}>
          <form onSubmit={form.handleSubmit(onSubmit)} className="space-y-8">
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
                        checked={
                          selectedOption === GroupAvailability.RESTRICTED
                        }
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
            <DialogFooter>
              <Button size="lg" variant="ring" disabled={loading}>
                Save
                {loading ? <Loader className="w-4 h-4 ml-2 spinner" /> : null}
              </Button>
            </DialogFooter>
          </form>
        </Form>
      </DialogContent>
    </Dialog>
  );
};
