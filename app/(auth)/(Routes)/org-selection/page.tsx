import { OrganizationList } from "@clerk/nextjs";

export default function Page() {
  return (
    <div>
      <div>
        <OrganizationList
          afterSelectOrganizationUrl="/"
          afterCreateOrganizationUrl="/"
          hidePersonal={true}
        />
      </div>
    </div>
  );
}
