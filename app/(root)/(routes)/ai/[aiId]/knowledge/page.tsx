"use client";
import { redirect } from "next/navigation";
import DataSourceCard from "./components/datasource-card";

const websiteUrlOnClick = (aiId: string) => {
  console.log("websiteUrlOnClick" + aiId);
};
const cloudStorageOnClick = (aiId: string) => {
  console.log("cloudStorageOnClick" + aiId);
  redirect(`/ai/${aiId}/knowledge/google-drive`);
};

interface SelectDataSourceProps {
  params: {
    aiId: string;
  };
}

const SelectDataSource = ({ params }: SelectDataSourceProps) => {
  console.log("aiId: " + params.aiId);
  const cloudStorageHref = `/ai/${params.aiId}/knowledge/google-drive`;
  return (
    <div className="container">
      <h1>Select a data source</h1>
      <p>Choose a data source for your data store</p>

      <div className="grid">
        <DataSourceCard
          title="Website URLs"
          description="Automatically crawl website content from a list of domains you define."
          href=""
        />
        <DataSourceCard
          title="Cloud Storage"
          description="Import data from a cloud storage bucket."
          href={cloudStorageHref}
        />
      </div>

      <style jsx>{`
        .container {
          max-width: 800px;
          margin: 0 auto;
          padding: 20px;
          text-align: center;
        }

        .grid {
          display: grid;
          grid-template-columns: repeat(2, 1fr);
          gap: 20px;
          margin-top: 20px;
        }
      `}</style>
    </div>
  );
};

export default SelectDataSource;
