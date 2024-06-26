"use client";

import { cn } from "@/src/lib/utils";
import { CldUploadButton } from "next-cloudinary";
import Image from "next/image";
import { useEffect, useState } from "react";

interface ImageUploadProps {
  value: string;
  onChange: (src: string) => void;
  disabled?: boolean;
  className?: string;
  width?: number;
  height?: number;
  loader?: (props: { src: string; width: number; quality?: any }) => string;
}

export const ImageUpload = ({
  value,
  onChange,
  disabled,
  className,
  width,
  height,
  loader,
}: ImageUploadProps) => {
  const [isMounted, setIsMounted] = useState(false);

  useEffect(() => {
    setIsMounted(true);
  }, []);

  if (!isMounted) {
    return false;
  }

  return (
    <div className="space-y-4 w-full flex flex-col justify-center items-center">
      <CldUploadButton
        options={{ maxFiles: 1 }}
        onUpload={(result: any) => onChange(result.info.secure_url)}
        uploadPreset="bawmwjeq"
      >
        <div
          className="
            p-4 
            border-4 
            border-dashed
            border-primary/10 
            rounded-lg 
            hover:opacity-75 
            transition 
            flex 
            flex-col 
            space-y-2 
            items-center 
            justify-center
          "
        >
          <div className={cn("relative h-40 w-40", className)}>
            <Image
              loader={loader}
              fill={!(width || height)}
              width={width}
              height={height}
              alt="Upload"
              src={value || "/placeholder.svg"}
              className="rounded-lg object-cover"
            />
          </div>
        </div>
      </CldUploadButton>
    </div>
  );
};
