"use client";

import { zodResolver } from "@hookform/resolvers/zod";
import axios from "axios";
import { useEffect, useState } from "react";
import { useForm } from "react-hook-form";

import { Button } from "@/components/ui/button";
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
import { Separator } from "@/components/ui/separator";
import { Textarea } from "@/components/ui/textarea";
import { useToast } from "@/components/ui/use-toast";
import { useGroupModal } from "@/hooks/use-group-modal";
import { CreateGroupRequest } from "@/src/domain/types/CreateGroupRequest";
import { UpdateGroupRequest } from "@/src/domain/types/UpdateGroupRequest";
import { useUser } from "@clerk/nextjs";
import { GroupAvailability } from "@prisma/client";
import { Loader, X } from "lucide-react";
import * as z from "zod";
import { useConfirmModal } from "@/hooks/use-confirm-modal";
import { Banner } from "./ui/banner";

const groupFormSchema = z.object({
  name: z.string().min(1, {
    message: "Name is required.",
  }),
  teammates: z.string(),
});

export const GroupModal = () => {
  const [isMounted, setIsMounted] = useState(false);
  const [loading, setLoading] = useState(false);
  const [selectedOption, setSelectedOption] =
    useState<GroupAvailability | null>(GroupAvailability.EVERYONE);
  const [currentTeammates, setCurrentTeammates] = useState<any[]>([]);
  const [removedTeammates, setRemovedTeammates] = useState<any[]>([]);
  const [isOwner, setIsOwner] = useState(true);
  const [search, setSearch] = useState("");
  const [filteredTeammates, setFilteredTeammates] = useState<any[]>([]);
  const { toast } = useToast();
  const { user } = useUser();
  const groupModal = useGroupModal();
  const confirmModal = useConfirmModal();

  const form = useForm<z.infer<typeof groupFormSchema>>({
    resolver: zodResolver(groupFormSchema),
    defaultValues: {
      name: "",
      teammates: "",
    },
  });

  useEffect(() => {
    setIsMounted(true);
  }, []);

  const fetchGroup = async () => {
    setLoading(true);
    const response = await axios.get(`/api/v1/groups/${groupModal.groupId}`);
    if (response.status === 200) {
      form.setValue("name", response.data.name);
      setSelectedOption(response.data.availability);
      setCurrentTeammates(response.data.users);
      setFilteredTeammates(response.data.users);
      setIsOwner(response.data.ownerUserId === user?.id);
    } else {
      toast({
        description: "Something went wrong",
        variant: "destructive",
      });
      groupModal.onClose();
    }
    setLoading(false);
  };

  useEffect(() => {
    if (groupModal.groupId) {
      fetchGroup();
    }
  }, [groupModal.groupId]);

  const updateGroup = async (values: z.infer<typeof groupFormSchema>) => {
    const request: UpdateGroupRequest = {
      name: values.name,
      availability: selectedOption || GroupAvailability.EVERYONE,
      memberEmailsToAdd: values.teammates,
      memberEmailsToRemove: removedTeammates,
    };

    const response = await axios.put(
      `/api/v1/groups/${groupModal.groupId}`,
      request
    );
    if (response.status === 200) {
      toast({
        description: "Group updated successfully",
      });
      groupModal.onUpdate(response.data);
      form.reset();
    } else {
      throw new Error(response.data.message);
    }
  };

  const createGroup = async (values: z.infer<typeof groupFormSchema>) => {
    const request: CreateGroupRequest = {
      name: values.name,
      availability: selectedOption || GroupAvailability.EVERYONE,
      memberEmails: values.teammates,
    };

    const response = await axios.post(`/api/v1/groups`, request);
    if (response.status === 200) {
      toast({
        description: "Group created successfully",
      });
      groupModal.onUpdate(response.data);
      form.reset();
    } else {
      throw new Error(response.data.message);
    }
  };

  const onSubmit = async (values: z.infer<typeof groupFormSchema>) => {
    try {
      setLoading(true);
      if (groupModal.groupId) {
        await updateGroup(values);
      } else {
        await createGroup(values);
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

  const handleDelete = async () => {
    setLoading(true);
    try {
      const response = await axios.delete(
        `/api/v1/groups/${groupModal.groupId}`
      );
      if (response.status === 200) {
        toast({
          description: "Group deleted successfully",
        });
        groupModal.onUpdate(response.data);
        form.reset();
      } else {
        throw new Error(response.data.message);
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

  const handleLeaveGroup = async () => {
    setLoading(true);
    try {
      const response = await axios.put(
        `/api/v1/me/groups/${groupModal.groupId}/leave`
      );
      if (response.status === 200) {
        toast({
          description: "You have left the group",
        });
        groupModal.onUpdate(response.data);
        form.reset();
      } else {
        throw new Error(response.data.message);
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

  const handleRemoveTeammate = (teammate: any) => {
    setCurrentTeammates((prevTeammates) =>
      prevTeammates.filter((t) => t.userId !== teammate.userId)
    );
    setRemovedTeammates((prevTeammates) => [...prevTeammates, teammate.email]);
  };

  if (!isMounted) {
    return null;
  }

  return (
    <Dialog
      open={groupModal.isOpen}
      onOpenChange={() => {
        form.reset();
        groupModal.onClose();
      }}
    >
      <DialogContent>
        <DialogHeader className="space-y-4">
          <DialogTitle className="text-center">
            {groupModal.groupId ? "Update" : "Create"} a Group
          </DialogTitle>
          <DialogDescription className="text-center space-y-2">
            {groupModal.groupId ? "Update" : "Create"} a group to share your AI
            with select people.
          </DialogDescription>
        </DialogHeader>
        <Separator />

        {loading ? (
          <div className="flex justify-center items-center h-32">
            <Loader className="w-16 h-16 spinner" />
          </div>
        ) : (
          <Form {...form}>
            <form onSubmit={form.handleSubmit(onSubmit)} className="space-y-4">
              {groupModal.groupId &&
                !isOwner &&
                selectedOption === GroupAvailability.RESTRICTED && (
                  <Banner>
                    Only the group owner can rename or delete this group.
                  </Banner>
                )}
              <FormField
                name="name"
                control={form.control}
                render={({ field }) => (
                  <FormItem>
                    <FormLabel>Group Name</FormLabel>
                    <FormControl>
                      <Input
                        placeholder="Group Name"
                        disabled={!isOwner || loading}
                        {...field}
                      />
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
                          disabled={!isOwner || loading}
                          value={GroupAvailability.EVERYONE}
                          checked={
                            selectedOption === GroupAvailability.EVERYONE
                          }
                          onChange={(e) =>
                            setSelectedOption(
                              e.target.value as GroupAvailability
                            )
                          }
                          className="mr-2"
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
                          disabled={!isOwner || loading}
                          value={GroupAvailability.RESTRICTED}
                          checked={
                            selectedOption === GroupAvailability.RESTRICTED
                          }
                          onChange={(e) =>
                            setSelectedOption(
                              e.target.value as GroupAvailability
                            )
                          }
                          className="mr-2"
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
                      <FormItem className="border-l border-ring pl-4">
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
                  {groupModal.groupId && (
                    <div>
                      <h4 className="mt-4">
                        Shared with {currentTeammates.length}{" "}
                        {currentTeammates.length === 1 ? "person" : "people"}
                      </h4>
                      {currentTeammates.length > 0 && (
                        <div className="rounded-md border mt-2">
                          {currentTeammates.length > 4 && (
                            <FormItem className="border-b">
                              <FormControl>
                                <Input
                                  placeholder="Search"
                                  disabled={loading}
                                  value={search}
                                  className="border-none"
                                  onChange={(e) => {
                                    setSearch(e.target.value);
                                    if (e.target.value === "") {
                                      setFilteredTeammates(currentTeammates);
                                    } else {
                                      setFilteredTeammates(
                                        currentTeammates.filter((teammate) =>
                                          teammate.email.includes(
                                            e.target.value
                                          )
                                        )
                                      );
                                    }
                                  }}
                                />
                              </FormControl>
                            </FormItem>
                          )}
                          <ul className="list-disc mt-2 max-h-44 overflow-auto">
                            {filteredTeammates.map((teammate) => (
                              <li
                                key={teammate.id}
                                className="flex justify-between items-center mb-2 border-b pl-2 last:border-b-0"
                              >
                                {teammate.email}
                                <button
                                  onClick={() => handleRemoveTeammate(teammate)}
                                  className="px-2 py-1"
                                >
                                  <X className="w-4 h-4" />
                                </button>
                              </li>
                            ))}
                          </ul>
                        </div>
                      )}
                    </div>
                  )}
                </>
              )}

              <DialogFooter>
                <div className="flex justify-between w-full">
                  <Button size="lg" variant="ring" disabled={loading}>
                    Save
                    {loading ? (
                      <Loader className="w-4 h-4 ml-2 spinner" />
                    ) : null}
                  </Button>

                  {groupModal.groupId && isOwner && (
                    <Button
                      size="lg"
                      variant="destructive"
                      onClick={() =>
                        confirmModal.onOpen(
                          "Delete Group?",
                          <div>
                            <div>
                              Are you sure you want to delete the{" "}
                              {form.getValues("name")} group?
                            </div>
                            <div>This action cannot be undone.</div>
                          </div>,
                          handleDelete
                        )
                      }
                      className="bg-red-600 hover:bg-red-700"
                      disabled={loading}
                      type="button"
                    >
                      Delete
                      {loading ? (
                        <Loader className="w-4 h-4 ml-2 spinner" />
                      ) : null}
                    </Button>
                  )}
                  {groupModal.groupId &&
                    !isOwner &&
                    selectedOption === GroupAvailability.RESTRICTED && (
                      <Button
                        size="lg"
                        variant="destructive"
                        onClick={() =>
                          confirmModal.onOpen(
                            "Leave Group?",
                            <div>
                              <div>
                                Are you sure you want to leave the{" "}
                                {form.getValues("name")} group?
                              </div>
                              <div>
                                To join again, you will need to ask the owner to
                                reinvite you.
                              </div>
                            </div>,
                            handleLeaveGroup
                          )
                        }
                        className="bg-red-600 hover:bg-red-700"
                        disabled={loading}
                        type="button"
                      >
                        Leave Group
                        {loading ? (
                          <Loader className="w-4 h-4 ml-2 spinner" />
                        ) : null}
                      </Button>
                    )}
                </div>
              </DialogFooter>
            </form>
          </Form>
        )}
      </DialogContent>
    </Dialog>
  );
};
