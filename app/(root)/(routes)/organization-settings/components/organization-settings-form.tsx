"use client";

import { zodResolver } from "@hookform/resolvers/zod";
import React, { useState } from "react";
import { SubmitHandler, useForm } from "react-hook-form";
import * as z from "zod";

import { Button } from "@/components/ui/button"; // Assuming a Button component is available
import {
  Dialog,
  DialogContent,
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

interface OrganizationSettingsFormProps {
  data: any;
}

const googleIntegrationFormSchema = z.object({
  clientId: z.string().min(1, "Client ID is required"),
  clientSecret: z.string().min(1, "Client Secret is required"),
});

interface GoogleIntegrationFormData {
  clientId: string;
  clientSecret: string;
}

export const OrganizationSettingsForm: React.FC<
  OrganizationSettingsFormProps
> = (data) => {
  const [showModal, setShowModal] = useState<boolean>(false);

  const form = useForm<GoogleIntegrationFormData>({
    resolver: zodResolver(googleIntegrationFormSchema),
    defaultValues: {
      clientId: "",
      clientSecret: "",
    },
  });

  const {
    control,
    handleSubmit,
    formState: { errors },
  } = useForm<GoogleIntegrationFormData>({
    resolver: zodResolver(googleIntegrationFormSchema),
  });

  const onSubmit: SubmitHandler<GoogleIntegrationFormData> = (data) => {
    console.log(data);
  };

  return (
    <div className="p-4 max-w-3xl mx-auto">
      <h1 className="text-lg font-medium">Data Source Integrations</h1>

      <div className="flex items-center justify-between my-4">
        <span>Google Drive</span>
        <Button
          type="button"
          variant="outline"
          onClick={() => setShowModal(true)}
        >
          Edit
        </Button>
      </div>

      {showModal && (
        <Dialog open={showModal} onOpenChange={setShowModal}>
          <DialogContent>
            <DialogHeader>
              <DialogTitle>Google Drive Integration Settings</DialogTitle>
            </DialogHeader>
            <Form {...form}>
              <form onSubmit={handleSubmit(onSubmit)} className="space-y-4">
                <FormField
                  name="clientId"
                  control={form.control}
                  render={({ field }) => (
                    <FormItem>
                      <FormLabel>Client ID</FormLabel>
                      <FormControl>
                        <Input placeholder="Client ID" {...field} />
                      </FormControl>
                      <FormMessage />
                    </FormItem>
                  )}
                />
                <FormField
                  name="clientSecret"
                  control={form.control}
                  render={({ field }) => (
                    <FormItem>
                      <FormLabel>Client Secret</FormLabel>
                      <FormControl>
                        <Input placeholder="Client Secret" {...field} />
                      </FormControl>
                      <FormMessage />
                    </FormItem>
                  )}
                />

                <div>
                  <label className="block mb-2 text-sm font-medium">
                    Scopes
                  </label>
                  <ul>
                    <li>https://www.googleapis.com/auth/userinfo.email</li>
                    <li>https://www.googleapis.com/auth/userinfo.profile</li>
                  </ul>
                </div>
                <DialogFooter>
                  <Button type="button" onClick={() => setShowModal(false)}>
                    Close
                  </Button>
                  <Button type="submit">Save Changes</Button>
                </DialogFooter>
              </form>
            </Form>
          </DialogContent>
        </Dialog>
      )}
    </div>
  );
};
