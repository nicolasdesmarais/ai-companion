import { cn } from "@/src/lib/utils";

const BlobAnimation = () => {
  const blobClasses = "absolute";
  return (
    <>
      <svg xmlns="http://www.w3.org/2000/svg" className="svg-filter">
        <defs>
          <filter id="goo">
            <feGaussianBlur
              in="SourceGraphic"
              stdDeviation="10"
              result="blur"
            />
            <feColorMatrix
              in="blur"
              mode="matrix"
              values="1 0 0 0 0  0 1 0 0 0  0 0 1 0 0  0 0 0 18 -8"
              result="goo"
            />
            <feBlend in="SourceGraphic" in2="goo" />
          </filter>
        </defs>
      </svg>
      <div className="h-full w-full blob-background overflow-hidden absolute">
        <div className={cn("blob1", blobClasses)}></div>
        <div className={cn("blob2", blobClasses)}></div>
        <div className={cn("blob3", blobClasses)}></div>
        <div className={cn("blob4", blobClasses)}></div>
        <div className={cn("blob5", blobClasses)}></div>
      </div>
    </>
  );
};

export default BlobAnimation;
