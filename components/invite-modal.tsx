"use client";

import { Dispatch, SetStateAction, useEffect, useState } from "react";
import { useForm } from "react-hook-form";
import { zodResolver } from "@hookform/resolvers/zod";
import axios from "axios";
import { Loader } from "lucide-react";

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
  FormMessage,
} from "@/components/ui/form";

import { Textarea } from "@/components/ui/textarea";
import { Button } from "@/components/ui/button";
import { Separator } from "@/components/ui/separator";
import { useToast } from "@/components/ui/use-toast";
import * as z from "zod";

const formSchema = z.object({
  emails: z.string().min(1, {
    message: "Invite email is required.",
  }),
});

interface InviteModalProps {
  showModal: boolean;
  setShowModal: Dispatch<SetStateAction<boolean>>;
}

export const InviteModal = ({ showModal, setShowModal }: InviteModalProps) => {
  const [isMounted, setIsMounted] = useState(false);
  const [loading, setLoading] = useState(false);
  const { toast } = useToast();

  const form = useForm<z.infer<typeof formSchema>>({
    resolver: zodResolver(formSchema),
    defaultValues: {
      emails: "",
    },
  });

  useEffect(() => {
    setIsMounted(true);
  }, []);

  const onSubmit = async (values: z.infer<typeof formSchema>) => {
    setLoading(true);
    try {
      await axios.post(`/api/v1/me/invitations`, {
        emails: values.emails,
      });
      form.setValue("emails", "");
      toast({
        description: "Invitations sent",
      });
      setShowModal(false);
    } catch (error) {
      console.error(error);
      toast({
        description: error.response?.data || "Something went wrong",
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
    <Dialog open={showModal} onOpenChange={() => setShowModal(false)}>
      <DialogContent>
        <Form {...form}>
          <form onSubmit={form.handleSubmit(onSubmit)} className="space-y-8">
            <DialogHeader className="space-y-4">
              <DialogTitle className="text-center">
                Invite people to join you in AppDirectAI
              </DialogTitle>
              <DialogDescription className="text-center space-y-2">
                Add teammates by email.
              </DialogDescription>
            </DialogHeader>
            <Separator />
            <FormField
              name="emails"
              control={form.control}
              render={({ field }) => (
                <FormItem className="col-span-2 md:col-span-1">
                  <FormControl>
                    <Textarea
                      disabled={loading}
                      placeholder="Ex: jennifer.wallace@acme.com, joe.hamm@acme.com"
                      {...field}
                    />
                  </FormControl>
                  <FormMessage />
                </FormItem>
              )}
            />
            <DialogFooter>
              <Button size="lg" disabled={loading} variant="ring">
                Invite
                {loading ? <Loader className="w-4 h-4 ml-2 spinner" /> : null}
              </Button>
            </DialogFooter>
          </form>
        </Form>
      </DialogContent>
    </Dialog>
  );
};
