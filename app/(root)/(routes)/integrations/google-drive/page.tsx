"use client";
import React from "react";

const ConnectDrive: React.FC = () => {
  const handleConnectClick = () => {
    // Redirect the user to the backend endpoint that initiates Google Drive OAuth
    window.location.href = "/api/integrations/google-drive/auth";
  };

  return (
    <div>
      <h1>Connect to Google Drive</h1>
      <p>Click the button below to connect your application to Google Drive.</p>
      <button onClick={handleConnectClick}>Connect to Google Drive</button>
    </div>
  );
};

export default ConnectDrive;
