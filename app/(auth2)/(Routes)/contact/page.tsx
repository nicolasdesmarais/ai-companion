"use client";
import BlobAnimation from "@/components/blob-animation";
import LandingNav from "@/components/landing-nav";
import LandingTerms from "@/components/landing-terms";
import { Button } from "@/components/ui/button";
import { Checkbox } from "@/components/ui/checkbox";
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
import { useState } from "react";
import { useForm } from "react-hook-form";
import * as z from "zod";

const formSchema = z.object({
  email: z.string().email("Invalid email address"),
  first: z.string().min(1, "First name is required"),
  last: z.string().min(1, "Last name is required"),
  company: z.string().min(1, "Company is required"),
  mlq: z.boolean(),
});

const Contact = () => {
  const form = useForm<z.infer<typeof formSchema>>({
    resolver: zodResolver(formSchema),
    defaultValues: {
      email: "",
      first: "",
      last: "",
      company: "",
      mlq: true,
    },
  });
  const { toast } = useToast();
  const [loading, setLoading] = useState(false);
  const [error, setError] = useState("");

  let host = "https://appdirect.ai",
    hutk = "";
  if (typeof window !== "undefined") {
    host = window.location.origin;
    hutk = document.cookie.replace(
      /(?:(?:^|.*;\s*)hubspotutk\s*\=\s*([^;]*).*$)|^.*$/,
      "$1"
    );
  }

  const onSubmit = async (values: z.infer<typeof formSchema>) => {
    setLoading(true);
    try {
      const data = {
        submittedAt: Date.now(),
        fields: [
          {
            objectTypeId: "0-1",
            name: "firstname",
            value: values.first,
          },
          {
            objectTypeId: "0-1",
            name: "lastname",
            value: values.last,
          },
          {
            objectTypeId: "0-1",
            name: "company",
            value: values.company,
          },
          {
            objectTypeId: "0-1",
            name: "email",
            value: values.email,
          },
          {
            objectTypeId: "0-1",
            name: "mql_form_flag",
            value: values.mlq,
          },
        ],
        context: {
          hutk,
          pageUri: `${host}/contact`,
          pageName: "AI for the Enterprise Contact Page",
        },
      };

      const resp = await axios.post(
        `https://api.hsforms.com/submissions/v3/integration/submit/43634300/6f8f8aaf-e95c-4372-b588-c546b5b2a715`,
        data
      );
      if (resp.status === 200) {
        form.setValue("email", "");
        form.setValue("first", "");
        form.setValue("last", "");
        form.setValue("company", "");
        form.setValue("mlq", false);
        toast({
          description: "Thank you, we will reach out soon.",
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
        <div className="bg-navylight md:bg-gradient4 z-10 rounded-lg flex flex-col items-center p-8 md:px-16 md:py-8 mx-2">
          <h1 className="text-3xl mb-12 font-bold">AI for the Enterprise</h1>
          <div>Connect with our Enterprise team today.</div>

          <div className="text-red-500 text-sm pt-4">{error}</div>
          <Form {...form}>
            <form
              onSubmit={form.handleSubmit(onSubmit)}
              className="flex flex-col gap-6 mt-8 w-full md:w-96"
            >
              <FormField
                name="first"
                control={form.control}
                render={({ field }) => (
                  <FormItem>
                    <FormControl>
                      <Input
                        {...field}
                        disabled={loading}
                        className="rounded-md h-12 px-4 bg-white focus-visible:ring-navylight ring-offset-navylight"
                        placeholder="First Name"
                      />
                    </FormControl>
                    <FormMessage />
                  </FormItem>
                )}
              />
              <FormField
                name="last"
                control={form.control}
                render={({ field }) => (
                  <FormItem>
                    <FormControl>
                      <Input
                        {...field}
                        disabled={loading}
                        className="rounded-md h-12 px-4 bg-white focus-visible:ring-navylight ring-offset-navylight"
                        placeholder="Last Name"
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
                        className="rounded-md h-12 px-4 bg-white focus-visible:ring-navylight ring-offset-navylight"
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
                        className="rounded-md h-12 px-4 bg-white focus-visible:ring-navylight ring-offset-navylight"
                        placeholder="Work email"
                      />
                    </FormControl>
                    <FormMessage />
                  </FormItem>
                )}
              />
              <FormField
                name="mlq"
                control={form.control}
                render={({ field }) => (
                  <FormItem>
                    <FormControl>
                      <Checkbox
                        id="mlq"
                        checked={field.value}
                        onCheckedChange={(val) => {
                          field.onChange(val);
                        }}
                        className="bg-white focus:bg-white focus:text-navy focus:ring-navylight ring-offset-navylight"
                      >
                        <div className="text-sm">
                          Check here if you wish to receive communications from
                          AppDirect about our new features and product updates.
                        </div>
                      </Checkbox>
                    </FormControl>
                  </FormItem>
                )}
              />
              <Button className="bg-white rounded-md px-16 py-2 text-center text-navy">
                Contact Us
                {loading ? <Loader className="w-4 h-4 ml-2 spinner" /> : null}
              </Button>
            </form>
            <LandingTerms className="mt-8" />
          </Form>
        </div>

        <BlobAnimation />
      </div>
    </div>
  );
};

export default Contact;
