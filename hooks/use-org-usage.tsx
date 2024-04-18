import axios from "axios";
import { create } from "zustand";

interface useOrgUsageStore {
  usage: any;
  fetchUsage: () => void;
}

let loading = false;

export const useOrgUsage = create<useOrgUsageStore>((set) => ({
  usage: {},
  fetchUsage: async () => {
    if (loading) return;
    loading = true;
    const result = await axios.get(`/api/v1/usage/org`);
    if (result.data) {
      set({ usage: result.data });
      loading = false;
    }
  },
}));
