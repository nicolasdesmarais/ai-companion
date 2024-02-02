"use client";

import { Table } from "@/components/table";
import { Button } from "@/components/ui/button";
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
import { Separator } from "@/components/ui/separator";
import { useToast } from "@/components/ui/use-toast";
import {
  CreateApiKeyRequest,
  CreateApiKeyResponse,
  ListApiKeyResponse,
} from "@/src/adapter-in/api/ApiKeysApi";
import { zodResolver } from "@hookform/resolvers/zod";
import axios, { AxiosError } from "axios";
import { format } from "date-fns";
import { Loader } from "lucide-react";
import { useState } from "react";
import { Controller, useForm } from "react-hook-form";
import * as z from "zod";

import { Checkbox } from "@/components/ui/checkbox";
import { isValidScope } from "@/src/security/models/Permission";
import Link from "next/link";

interface APIKeysFormProps {
  userScopes: string[];
  initialApiKeys: ListApiKeyResponse[];
}

interface NewAPIKeyFormData {
  name: string;
  scopes: string[];
}

const apiKeyFormSchema = z.object({
  name: z.string().min(1, { message: "Name is required." }),
  scopes: z
    .array(z.string())
    .nonempty({ message: "At least one scope is required." }),
});

export const APIKeysForm: React.FC<APIKeysFormProps> = ({
  userScopes,
  initialApiKeys,
}) => {
  const { toast } = useToast();
  const [apiKeys, setApiKeys] = useState<ListApiKeyResponse[]>(initialApiKeys);
  const [editingKey, setEditingKey] = useState<ListApiKeyResponse | null>(null);
  const [isModalOpen, setIsModalOpen] = useState(false);
  const [isEditModalOpen, setIsEditModalOpen] = useState(false);
  const [loading, setLoading] = useState(false);
  const [createdKey, setCreatedKey] = useState<CreateApiKeyResponse>();

  const form = useForm<NewAPIKeyFormData>({
    resolver: zodResolver(apiKeyFormSchema),
    defaultValues: {
      name: "",
      scopes: [],
    },
  });

  const openModal = () => {
    form.reset();
    setIsModalOpen(true);
  };
  const closeModal = () => {
    form.reset();
    setIsModalOpen(false);
    setCreatedKey(undefined);
  };

  const openEditModal = (listApiKeyResponse: ListApiKeyResponse) => {
    form.setValue("name", listApiKeyResponse.name);
    form.setValue("scopes", listApiKeyResponse.scopes);

    setEditingKey(listApiKeyResponse);
    setIsEditModalOpen(true);
  };
  const closeEditModal = () => {
    setEditingKey(null);
    setIsEditModalOpen(false);
  };

  const onCreateKey = async (values: NewAPIKeyFormData) => {
    try {
      setLoading(true);

      const request: CreateApiKeyRequest = {
        name: values.name,
        scopes: values.scopes,
      };

      const apiKey = await axios.post("/api/v1/api-keys", request);
      setApiKeys([...apiKeys, apiKey.data]);
      setCreatedKey(apiKey.data);
    } catch (error) {
      toast({
        description: "Something went wrong",
        variant: "destructive",
      });
    } finally {
      setLoading(false);
    }
  };

  const onUpdateKey = async (data: NewAPIKeyFormData) => {
    if (!editingKey) return;

    try {
      setLoading(true);

      const updatePayload = {
        name: data.name,
        scopes: data.scopes,
      };

      const updatedKey = await axios.put(
        `/api/v1/api-keys/${editingKey.id}`,
        updatePayload
      );

      setApiKeys(
        apiKeys.map((k) => (k.id === editingKey.id ? updatedKey.data : k))
      );
      setEditingKey(null); // Reset the editing key
    } catch (error) {
      toast({
        description: "Something went wrong",
        variant: "destructive",
      });
    } finally {
      setLoading(false);
      setIsModalOpen(false); // Close the modal after update
    }
  };

  const handleDelete = async (keyId: string) => {
    try {
      await axios.delete(`/api/v1/api-keys/${keyId}`);
      setApiKeys(apiKeys.filter((key) => key.id !== keyId));
      toast({ description: "API key deleted successfully." });
    } catch (error: any) {
      toast({
        variant: "destructive",
        description:
          String((error as AxiosError).response?.data) ||
          "Something went wrong.",
        duration: 6000,
      });
    }
  };

  return (
    <div className="h-full p-4 max-w-3xl mx-auto">
      <h1 className="text-lg font-medium">API Docs</h1>
      <p className="text-sm text-muted-foreground">
        There are several APIs available for your use. You can try them out on{" "}
        <Link href="/api-doc" className="cursor-pointer text-ring">
          the interactive API listing page
        </Link>
        . Authenticate by passing your generated secret key in the header of the
        request:
      </p>
      <p className="text-xs my-2 whitespace-pre font-mono p-4 bg-primary/10 rounded-md overflow-auto">
        {
          "curl https://appdirect.ai/api/v1/me/ai -H 'X-Authorization: Bearer <your-secret-key>'"
        }
      </p>

      <Separator className="bg-primary/10 my-4" />
      <h1 className="text-lg font-medium">Your API Keys</h1>
      <p className="text-sm text-muted-foreground">
        Your secret API keys are shown here. Remember, the secret keys are not
        displayed again once generated.
      </p>
      <Table
        headers={["Name", "Created At", "Scopes", "Action"]}
        className="w-full my-4 mr-4"
      >
        {apiKeys.map((key) => (
          <tr key={key.id} className="text-sm">
            <td className="p-2">{key.name}</td>
            <td className="p-2">
              {key.createdAt
                ? format(new Date(key.createdAt), "h:mma M/d/yyyy ")
                : null}
            </td>
            <td className="p-2">
              {key.scopes.map((scope) => (
                <div key={`${key.name}-${scope}`}>{scope}</div>
              ))}
            </td>
            <td className="p-2">
              <Button
                type="button"
                variant="outline"
                className="m-2"
                onClick={() => openEditModal(key)}
              >
                Edit
              </Button>
              <Button
                type="button"
                variant="outline"
                onClick={() => handleDelete(key.id)}
              >
                Delete
              </Button>
            </td>
          </tr>
        ))}
      </Table>
      <div className="mt-4">
        <Button onClick={openModal}>Create new secret key</Button>
      </div>

      {/* Modal for creating a new API Key */}
      <Dialog open={isModalOpen} onOpenChange={setIsModalOpen}>
        <DialogContent>
          <DialogHeader className="space-y-4">
            <DialogTitle className="text-center">
              Create New Secret Key
            </DialogTitle>
          </DialogHeader>
          <Separator />

          {loading ? (
            <div className="flex justify-center items-center h-32">
              <Loader className="w-16 h-16 spinner" />
            </div>
          ) : createdKey ? (
            <div className="space-y-2">
              <div className="text-center font-mono text-sm overflow-x-auto whitespace-nowrap">
                {createdKey.key}
              </div>
              <div className="text-center">
                Remember, the secret keys are not displayed again once
                generated.
              </div>
              <div className="mt-4 flex justify-center">
                <Button onClick={closeModal}>Close</Button>
              </div>
            </div>
          ) : (
            <Form {...form}>
              <form
                onSubmit={form.handleSubmit(onCreateKey)}
                className="space-y-4"
              >
                <FormField
                  name="name"
                  control={form.control}
                  render={({ field }) => (
                    <FormItem>
                      <FormLabel>Name</FormLabel>
                      <FormControl>
                        <Input placeholder="Name" {...field} />
                      </FormControl>
                      <FormMessage />
                    </FormItem>
                  )}
                />
                <div>
                  <FormLabel>Scopes</FormLabel>
                  {userScopes.map((scope) => (
                    <Controller
                      key={scope}
                      name="scopes"
                      control={form.control}
                      render={({ field }) => (
                        <Checkbox
                          checked={field.value.includes(scope)}
                          onCheckedChange={(isChecked) => {
                            if (isChecked) {
                              field.onChange([...field.value, scope]);
                            } else {
                              field.onChange(
                                field.value.filter((s) => s !== scope)
                              );
                            }
                          }}
                        >
                          {scope}
                        </Checkbox>
                      )}
                    />
                  ))}
                </div>
                <DialogFooter>
                  <Button variant="outline" onClick={closeModal}>
                    Cancel
                  </Button>
                  <Button type="submit">Create Secret Key</Button>
                </DialogFooter>
              </form>
            </Form>
          )}
        </DialogContent>
      </Dialog>

      {editingKey && (
        <Dialog open={isEditModalOpen} onOpenChange={setIsEditModalOpen}>
          <DialogContent>
            <DialogHeader className="space-y-4">
              <DialogTitle className="text-center">Update API Key</DialogTitle>
            </DialogHeader>
            <Separator />
            <Form {...form}>
              <form
                onSubmit={form.handleSubmit(onUpdateKey)}
                className="space-y-4"
              >
                <FormField
                  name="name"
                  control={form.control}
                  render={({ field }) => (
                    <FormItem>
                      <FormLabel>Name</FormLabel>
                      <FormControl>
                        <Input {...field} />
                      </FormControl>
                      <FormMessage />
                    </FormItem>
                  )}
                />
                <div>
                  <FormLabel>Scopes</FormLabel>
                  {userScopes.map((scope) => (
                    <Controller
                      key={scope}
                      name="scopes"
                      control={form.control}
                      render={({ field }) => (
                        <Checkbox
                          checked={field.value.includes(scope)}
                          onCheckedChange={(isChecked) => {
                            if (isChecked) {
                              field.onChange([...field.value, scope]);
                            } else {
                              field.onChange(
                                field.value.filter((s) => s !== scope)
                              );
                            }
                          }}
                        >
                          {scope}
                        </Checkbox>
                      )}
                    />
                  ))}
                </div>
                <DialogFooter>
                  <Button variant="outline" onClick={closeEditModal}>
                    Cancel
                  </Button>
                  <Button type="submit">Update API Key</Button>
                </DialogFooter>
              </form>
            </Form>
          </DialogContent>
        </Dialog>
      )}
    </div>
  );
};
