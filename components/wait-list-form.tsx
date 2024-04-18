"use client";
import { useForm } from "react-hook-form";
import * as z from "zod";
import { zodResolver } from "@hookform/resolvers/zod";
import { Form } from "@/components/ui/form";
import {
  FormControl,
  FormField,
  FormItem,
  FormMessage,
} from "@/components/ui/form";
import { Input } from "@/components/ui/input";
import { Button } from "@/components/ui/button";
import axios from "axios";
import { useToast } from "./ui/use-toast";
import { useState } from "react";
import { Loader } from "lucide-react";

const formSchema = z.object({
  email: z.string().email("Invalid email address"),
  name: z.string().min(1, "Name is required"),
  company: z.string().min(1, "Company is required"),
});

export const WaitListForm = () => {
  const form = useForm<z.infer<typeof formSchema>>({
    resolver: zodResolver(formSchema),
  });
  const { toast } = useToast();
  const [loading, setLoading] = useState(false);

  const onSubmit = async (values: z.infer<typeof formSchema>) => {
    setLoading(true);
    try {
      const resp = await axios.post("/api/v1/waitlist", values);
      if (resp.status === 200) {
        form.setValue("email", "");
        form.setValue("name", "");
        form.setValue("company", "");
        toast({
          description: "Thank you for signing up!",
          duration: 3000,
        });
      } else {
        toast({
          description: "Something went wrong.",
          variant: "destructive",
          duration: 3000,
        });
      }
    } catch (error) {
      toast({
        description: error.response?.data || "Something went wrong.",
        variant: "destructive",
        duration: 3000,
      });
    }
    setLoading(false);
  };
  return (
    <Form {...form}>
      <form onSubmit={form.handleSubmit(onSubmit)} className="pb-10">
        <div className="grid grid-cols-1 md:grid-cols-2 gap-4 m-4">
          <FormField
            name="name"
            control={form.control}
            render={({ field }) => (
              <FormItem className="col-span-2">
                <FormControl>
                  <Input
                    {...field}
                    disabled={loading}
                    className="rounded-none"
                    placeholder="Full Name"
                  />
                </FormControl>
                <FormMessage />
              </FormItem>
            )}
          />
          <FormField
            name="email"
            control={form.control}
            render={({ field }) => (
              <FormItem className="col-span-2 md:col-span-1">
                <FormControl>
                  <Input
                    {...field}
                    disabled={loading}
                    className="rounded-none"
                    placeholder="Work Email"
                  />
                </FormControl>
                <FormMessage />
              </FormItem>
            )}
          />
          <FormField
            name="company"
            control={form.control}
            render={({ field }) => (
              <FormItem className="col-span-2 md:col-span-1">
                <FormControl>
                  <Input
                    {...field}
                    disabled={loading}
                    className="rounded-none"
                    placeholder="Company Name"
                  />
                </FormControl>
                <FormMessage />
              </FormItem>
            )}
          />
          <Button className="mt-5 rounded-none col-span-2" variant="ring">
            Sign me up
            {loading ? <Loader className="w-4 h-4 spinner ml-2" /> : null}
          </Button>
        </div>
      </form>
    </Form>
  );
};
