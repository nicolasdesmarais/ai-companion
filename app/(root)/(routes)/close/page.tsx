"use client";
import React, { useEffect } from "react";

const ClosePage = () => {
  useEffect(() => {
    window.close();
  }, []);

  return null;
};

export default ClosePage;
