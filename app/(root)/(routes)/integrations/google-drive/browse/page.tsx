import { currentUser } from "@clerk/nextjs";
import { BrowseGoogleDrive } from "./components/browse-google-drive";

// Mock data for files

const BrowseGoogleDrivePage = async () => {
  const user = await currentUser();
  if (!user) {
    return;
  }

  return (
    <div className="container mx-auto mt-10">
      <h1 className="text-2xl mb-4">Search Files</h1>
      <BrowseGoogleDrive userId={user.id} />
    </div>
  );
};

export default BrowseGoogleDrivePage;
