"use client";

import React, { useState } from "react";

// Define a type for the API key
type APIKey = {
  id: number;
  name: string;
  createdAt: string;
  lastUsed: string;
};

const APIKeysForm: React.FC = () => {
  // Mock data array with type definition
  const [apiKeys, setApiKeys] = useState<APIKey[]>([
    { id: 1, name: "Key 1", createdAt: "2022-01-01", lastUsed: "2022-03-01" },
    { id: 2, name: "Key 2", createdAt: "2022-02-01", lastUsed: "2022-04-01" },
    // ... more keys
  ]);

  // Function to handle deletion of a key
  const handleDelete = (keyId: number): void => {
    setApiKeys(apiKeys.filter((key) => key.id !== keyId));
  };

  // Function to handle creation of a new key (mock function)
  const createNewKey = (): void => {
    // Logic to create a new key
    alert("Create new key functionality not implemented");
  };

  return (
    <div>
      <h1>API Keys</h1>
      <p>
        Your secret API keys are shown here. Remember, the secret keys are not
        displayed again once generated.
      </p>
      <table>
        <thead>
          <tr>
            <th>Name</th>
            <th>Created At</th>
            <th>Last Used</th>
            <th>Action</th>
          </tr>
        </thead>
        <tbody>
          {apiKeys.map((key: APIKey) => (
            <tr key={key.id}>
              <td>{key.name}</td>
              <td>{key.createdAt}</td>
              <td>{key.lastUsed}</td>
              <td>
                <button onClick={() => handleDelete(key.id)}>Delete</button>
              </td>
            </tr>
          ))}
        </tbody>
      </table>
      <button onClick={createNewKey}>Create new secret key</button>
    </div>
  );
};

export default APIKeysForm;
