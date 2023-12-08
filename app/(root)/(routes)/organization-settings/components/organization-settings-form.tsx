import { IconGear } from "@/components/icons"; // Assuming an Icon component for the gear icon
import {
  Dialog,
  DialogContent,
  DialogFooter,
  DialogHeader,
  DialogTitle,
} from "@/components/ui/dialog";
import { Input } from "@/components/ui/input";
import { Switch } from "@/components/ui/switch"; // Assuming a Switch component is available
import { useState } from "react";
import { Controller, useForm } from "react-hook-form";

export const OrganizationSettingsForm = () => {
  const [isGoogleEnabled, setGoogleEnabled] = useState(false);
  const [showModal, setShowModal] = useState(false);
  const { control } = useForm();

  return (
    <div className="p-4 max-w-3xl mx-auto">
      <h1 className="text-lg font-medium">Settings</h1>
      <p className="text-sm text-muted-foreground">Integrations</p>

      <div className="flex items-center justify-between my-4">
        <span>Google</span>
        <Switch checked={isGoogleEnabled} onChange={setGoogleEnabled} />
        <button
          disabled={!isGoogleEnabled}
          onClick={() => setShowModal(true)}
          className={`icon-button ${!isGoogleEnabled ? "disabled" : ""}`}
        >
          <IconGear />
        </button>
      </div>

      {showModal && (
        <Dialog open={showModal} onOpenChange={setShowModal}>
          <DialogContent>
            <DialogHeader>
              <DialogTitle>Google Integration Settings</DialogTitle>
            </DialogHeader>
            <form className="space-y-4">
              <Controller
                name="clientId"
                control={control}
                render={({ field }) => <Input label="Client ID" {...field} />}
              />
              <Controller
                name="clientSecret"
                control={control}
                render={({ field }) => (
                  <Input label="Client Secret" {...field} />
                )}
              />
              <Input label="Redirect URI" value="www.appdirect.ai" disabled />
              <div>
                <label className="block mb-2 text-sm font-medium">Scopes</label>
                <ul>
                  <li>https://www.googleapis.com/auth/userinfo.email</li>
                  <li>https://www.googleapis.com/auth/userinfo.profile</li>
                </ul>
              </div>
              <DialogFooter>
                <Button onClick={() => setShowModal(false)}>Close</Button>
              </DialogFooter>
            </form>
          </DialogContent>
        </Dialog>
      )}
    </div>
  );
};
