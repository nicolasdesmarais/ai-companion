"use client";
import React from "react";

const ConnectDrive: React.FC = () => {
  const handleConnectClick = () => {
    // Redirect the user to the backend endpoint that initiates Google Drive OAuth
    window.location.href = "/api/v1/integrations/google-drive/auth";
  };

  const buttonStyle = {
    backgroundColor: "#4285F4",
    border: "none",
    borderRadius: "4px",
    padding: "10px 20px",
    color: "white",
    fontSize: "16px",
    cursor: "pointer",
    display: "flex",
    alignItems: "center",
    justifyContent: "center",
  };

  const iconStyle = {
    marginRight: "8px",
  };

  return (
    <div>
      <button onClick={handleConnectClick} style={buttonStyle}>
        Connect to Google Drive
      </button>
    </div>
  );
};

export default ConnectDrive;
