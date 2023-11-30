"use client";
import {
  FormControl,
  FormDescription,
  FormField,
  FormItem,
  FormLabel,
  FormMessage,
} from "@/components/ui/form";
import { Separator } from "@/components/ui/separator";
import { Slider } from "@/components/ui/slider";
import { AIModel } from "@/src/domain/models/AIModel";
import { Prisma } from "@prisma/client";

const extendedAI = Prisma.validator<Prisma.AIDefaultArgs>()({
  include: {
    dataSources: {
      include: {
        dataSource: true,
      },
    },
  },
});

type ExtendedAI = Prisma.AIGetPayload<typeof extendedAI>;
interface ProfileSourceProps {
  initialAi: ExtendedAI | null;
  form: any;
  aiModels: AIModel[];
}

export const AIProfile = ({
  initialAi,
  form,
  aiModels,
}: ProfileSourceProps) => {
  const isLoading = form.formState.isSubmitting;

  return (
    <div className="h-full p-4 space-y-2 max-w-3xl mx-auto">
      <div className="space-y-2 w-full col-span-2">
        <div>
          <h3 className="text-lg font-medium">Profile Information</h3>
          <p className="text-sm text-muted-foreground">
            Text and images to display in the catalog of AIs.
          </p>
        </div>
        <Separator className="bg-primary/10" />
      </div>
    </div>
  );
};
