"use client";

import { useState } from "react";

const CreateGroupForm = () => {
  const [groupName, setGroupName] = useState("");
  const [availability, setAvailability] = useState("Everyone in your Company");
  const [teammates, setTeammates] = useState("");

  const handleChange = (event: { target: { name: any; value: any } }) => {
    const { name, value } = event.target;

    switch (name) {
      case "groupName":
        setGroupName(value);
        break;
      case "availability":
        setAvailability(value);
        break;
      case "teammates":
        setTeammates(value);
        break;
      default:
        break;
    }
  };

  const handleSubmit = (event: { preventDefault: () => void }) => {
    event.preventDefault();

    // Validate the form data

    // Submit the form data to a backend server
  };

  return (
    <form onSubmit={handleSubmit}>
      <label>
        Group Name:
        <input
          type="text"
          name="groupName"
          value={groupName}
          onChange={handleChange}
        />
      </label>
      <label>
        Availability:
        <select
          name="availability"
          value={availability}
          onChange={handleChange}
        >
          <option value="Everyone in your Company">
            Everyone in your Company
          </option>
          <option value="Specific teams">Specific team members</option>
        </select>
      </label>
      <label>
        Team Members (optional):
        <textarea name="teammates" value={teammates} onChange={handleChange} />
      </label>
      <button type="submit">Create</button>
    </form>
  );
};

export default CreateGroupForm;
