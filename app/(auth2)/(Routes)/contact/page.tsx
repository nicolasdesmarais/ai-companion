"use client";
import BlobAnimation from "@/components/blob-animation";
import LandingNav from "@/components/landing-nav";
import { Button } from "@/components/ui/button";
import {
  Form,
  FormControl,
  FormField,
  FormItem,
  FormMessage,
} from "@/components/ui/form";
import { Input } from "@/components/ui/input";
import { useToast } from "@/components/ui/use-toast";
import { zodResolver } from "@hookform/resolvers/zod";
import axios from "axios";
import { Loader } from "lucide-react";
import { useRouter } from "next/navigation";
import { useState } from "react";
import { useForm } from "react-hook-form";
import * as z from "zod";

const formSchema = z.object({
  email: z.string().email("Invalid email address"),
  name: z.string().min(1, "Name is required"),
  company: z.string().min(1, "Company is required"),
});

const Contact = () => {
  const form = useForm<z.infer<typeof formSchema>>({
    resolver: zodResolver(formSchema),
  });
  const { toast } = useToast();
  const [loading, setLoading] = useState(false);
  const [emailAddress, setEmailAddress] = useState("");
  const [name, setName] = useState("");
  const [company, setCompany] = useState("");
  const [error, setError] = useState("");
  const router = useRouter();

  const onSubmit = async (values: z.infer<typeof formSchema>) => {
    setLoading(true);
    try {
      const resp = await axios.post("/api/v1/contact", values);
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
    <div className="bg-white flex flex-col text-navy h-screen">
      <LandingNav transparent />

      <div className="h-full w-full flex items-center justify-center">
        <div className="bg-navylight md:bg-gradient4 z-10 rounded-lg flex flex-col items-center p-8 md:p-16 mx-2">
          <h1 className="text-3xl mb-12 font-bold">AI for the Enterprise</h1>
          <div>Connect with our Enterprise team today.</div>
          <div className="text-red-500 text-sm pt-4">{error}</div>
          <Form {...form}>
            <form
              onSubmit={form.handleSubmit(onSubmit)}
              className="pb-10 flex flex-col gap-8 mt-8 w-full"
            >
              <FormField
                name="name"
                control={form.control}
                render={({ field }) => (
                  <FormItem>
                    <FormControl>
                      <Input
                        {...field}
                        disabled={loading}
                        className="rounded-md md:w-80 h-12 px-4 bg-white focus-visible:ring-navylight ring-offset-navylight"
                        placeholder="Name"
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
                  <FormItem>
                    <FormControl>
                      <Input
                        {...field}
                        disabled={loading}
                        className="rounded-md md:w-80 h-12 px-4 bg-white focus-visible:ring-navylight ring-offset-navylight"
                        placeholder="Company"
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
                  <FormItem>
                    <FormControl>
                      <Input
                        {...field}
                        disabled={loading}
                        className="rounded-md md:w-80 h-12 px-4 bg-white focus-visible:ring-navylight ring-offset-navylight"
                        placeholder="Work email"
                      />
                    </FormControl>
                    <FormMessage />
                  </FormItem>
                )}
              />

              <Button className="bg-white rounded-md px-16 py-2 text-center text-navy">
                Contact Us
                {loading ? <Loader className="w-4 h-4 ml-2 spinner" /> : null}
              </Button>
            </form>
          </Form>
        </div>

        <BlobAnimation />
      </div>
    </div>
  );
};

export default Contact;
