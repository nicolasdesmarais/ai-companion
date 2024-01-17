import { cn } from "@/src/lib/utils";

interface Props {
  fill?: string;
  className?: string;
}

export const GoogleDriveSvg = ({ fill, className }: Props) => {
  return (
    <svg
      xmlns="http://www.w3.org/2000/svg"
      role="img"
      aria-label="googleDriveIcon"
      viewBox="0 0 24 24"
      className={cn(className)}
      stroke="none"
      fill={fill || "currentColor"}
    >
      <path
        id="Shape"
        d="M13.142,10.481,6.805,0H0L6.338,10.481Z"
        transform="translate(8.282 3.523)"
      />
      <path
        id="Shape-2"
        data-name="Shape"
        d="M5.838,0,0,10.116l3.4,5.892L9.24,5.9Z"
        transform="translate(2 4.553)"
      />
      <path
        id="Shape-3"
        data-name="Shape"
        d="M3.654,0,0,6.406H11.777L15.18,0Z"
        transform="translate(6.211 15.079)"
      />
      <rect id="Rectangle" width="24" height="24" fill="none" />
    </svg>
  );
};
