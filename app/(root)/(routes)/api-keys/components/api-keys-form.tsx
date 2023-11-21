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
  ApiScope,
  CreateApiKeyRequest,
  CreateApiKeyResponse,
  ListApiKeyResponse,
} from "@/src/ports/api/ApiKeysApi";
import { zodResolver } from "@hookform/resolvers/zod";
import axios, { AxiosError } from "axios";
import { format } from "date-fns";
import { Loader } from "lucide-react";
import { useState } from "react";
import { useForm } from "react-hook-form";
import * as z from "zod";

import { Checkbox } from "@/components/ui/checkbox";
import { Controller } from "react-hook-form";

interface APIKeysFormProps {
  initialApiKeys: ListApiKeyResponse[];
}

interface NewAPIKeyFormData {
  name: string;
  scopes: ApiScope[];
}

const apiKeyFormSchema = z.object({
  name: z.string().min(1, { message: "Name is required." }),
  scopes: z
    .array(z.nativeEnum(ApiScope))
    .nonempty({ message: "At least one scope is required." }),
});

export const APIKeysForm: React.FC<APIKeysFormProps> = ({ initialApiKeys }) => {
  const { toast } = useToast();
  const [apiKeys, setApiKeys] = useState<ListApiKeyResponse[]>(initialApiKeys);
  const [isModalOpen, setIsModalOpen] = useState(false);
  const [loading, setLoading] = useState(false);
  const [createdKey, setCreatedKey] = useState<CreateApiKeyResponse>();

  const form = useForm<NewAPIKeyFormData>({
    resolver: zodResolver(apiKeyFormSchema),
    defaultValues: {
      name: "",
      scopes: [],
    },
  });

  const openModal = () => setIsModalOpen(true);
  const closeModal = () => {
    form.reset();
    setIsModalOpen(false);
    setCreatedKey(undefined);
  };

  const renderScopes = (scopes: ApiScope[]) => {
    return scopes.join(", ");
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
      <h1 className="text-lg font-medium">API Keys</h1>
      <p className="text-sm text-muted-foreground">
        Your secret API keys are shown here. Remember, the secret keys are not
        displayed again once generated.
      </p>
      <Table
        headers={["Name", "Created At", "Scopes", "Action"]}
        className="w-full my-4"
      >
        {apiKeys.map((key) => (
          <tr key={key.id} className="text-sm">
            <td className="p-2">{key.name}</td>
            <td className="p-2">
              {key.createdAt
                ? format(new Date(key.createdAt), "h:mma M/d/yyyy ")
                : null}
            </td>
            <td className="p-2">{renderScopes(key.scopes)}</td>
            <td className="p-2">
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
            <div>
              <div>{createdKey.key}</div>
              <div>
                Remember, the secret keys are not displayed again once
                generated.
              </div>
              <div className="mt-4">
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
                  {Object.values(ApiScope).map((scope) => (
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
                                field.value.filter((s: ApiScope) => s !== scope)
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
    </div>
  );
};
