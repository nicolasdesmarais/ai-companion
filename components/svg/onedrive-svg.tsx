import { cn } from "@/src/lib/utils";

interface Props {
  fill?: string;
  className?: string;
}

export const OneDriveSvg = ({ fill, className }: Props) => {
  return (
    <svg
      xmlns="http://www.w3.org/2000/svg"
      role="img"
      aria-label="OneDriveIcon"
      viewBox="0 0 24 24"
      className={cn(className)}
      stroke="none"
      fill={fill || "currentColor"}
    >
      <g stroke-width="1" fill-rule="evenodd">
        <g transform="translate(-220.000000, -7519.000000)">
          <g transform="translate(56.000000, 160.000000)">
            <path d="M174,7379 L184,7379 L184,7370 L174,7370 L174,7379 Z M164,7379 L173,7379 L173,7370 L164,7370 L164,7379 Z M174,7369 L184,7369 L184,7359 L174,7359 L174,7369 Z M164,7369 L173,7369 L173,7359 L164,7359 L164,7369 Z"></path>
          </g>
        </g>
      </g>
    </svg>
  );
};
