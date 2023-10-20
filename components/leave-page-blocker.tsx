import { useEffect } from "react";

const LeavePageBlocker = ({ when }: { when: boolean }) => {
  useEffect(() => {
    if (!when) return () => {};

    const message = "You have unsaved changes, are you sure you want to leave?";
    const beforeUnloadCallback = (event: any) => {
      event.preventDefault();
      event.returnValue = message;
      return message;
    };
    window.addEventListener("beforeunload", beforeUnloadCallback);
    return () => {
      window.removeEventListener("beforeunload", beforeUnloadCallback);
    };
  }, [when]);
  return null;
};

export default LeavePageBlocker;
