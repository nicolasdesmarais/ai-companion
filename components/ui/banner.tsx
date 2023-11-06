import { Info } from "lucide-react";

interface BannerProps {
  text: string;
}

export const Banner = ({ text }: BannerProps) => {
  return (
    <div className="flex bg-accent p-2 rounded-lg">
      <Info className="h-5 w-5" />
      <p className="text-sm text-left ml-2">{text}</p>
    </div>
  );
};
