"use client";

import { zodResolver } from "@hookform/resolvers/zod";
import React, { useState } from "react";
import { useForm } from "react-hook-form";
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
import { toast } from "@/components/ui/use-toast";
import { UpsertClientCredentialsRequest } from "@/src/domain/ports/api/OrgClientCredentialsApi";
import { OAuthTokenProvider } from "@prisma/client";
import axios from "axios";
import { Loader } from "lucide-react";

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
> = ({ data }) => {
  const [showModal, setShowModal] = useState<boolean>(false);
  const [loading, setLoading] = useState(false);
  const [googleIntegrationData, setGoogleIntegrationData] =
    useState<GoogleIntegrationFormData>(data);

  const form = useForm<GoogleIntegrationFormData>({
    resolver: zodResolver(googleIntegrationFormSchema),
    defaultValues: {
      clientId: "",
      clientSecret: "",
    },
  });

  const openModal = () => {
    form.reset();

    form.setValue("clientId", googleIntegrationData?.clientId);
    form.setValue("clientSecret", googleIntegrationData?.clientSecret);

    setShowModal(true);
  };
  const onSaveGoogleDrive = async (values: GoogleIntegrationFormData) => {
    try {
      setLoading(true);

      const updatedData = {
        clientId: values.clientId,
        clientSecret: values.clientSecret,
      };

      setGoogleIntegrationData(updatedData);

      const request: UpsertClientCredentialsRequest = {
        provider: OAuthTokenProvider.GOOGLE,
        data: updatedData,
      };

      const apiKey = await axios.post(
        "/api/v1/org-client-credentials",
        request
      );
    } catch (error) {
      toast({
        description: "Something went wrong",
        variant: "destructive",
      });
    } finally {
      setLoading(false);
    }
  };

  return (
    <div className="p-4 max-w-3xl mx-auto">
      <h1 className="text-lg font-medium">Data Source Integrations</h1>

      <div className="flex items-center justify-between my-4">
        <span>Google Drive</span>
        <Button type="button" variant="outline" onClick={() => openModal()}>
          Edit
        </Button>
      </div>

      {showModal && (
        <Dialog open={showModal} onOpenChange={setShowModal}>
          <DialogContent>
            <DialogHeader>
              <DialogTitle>Google Drive Integration Settings</DialogTitle>
            </DialogHeader>
            {loading ? (
              <div className="flex justify-center items-center h-32">
                <Loader className="w-16 h-16 spinner" />
              </div>
            ) : (
              <Form {...form}>
                <form
                  onSubmit={form.handleSubmit(onSaveGoogleDrive)}
                  className="space-y-4"
                >
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
            )}
          </DialogContent>
        </Dialog>
      )}
    </div>
  );
};
