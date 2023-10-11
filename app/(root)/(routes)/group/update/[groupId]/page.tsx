import { ClerkService } from "@/domain/services/ClerkService";
import { GroupService } from "@/domain/services/GroupService";
import { Utilities } from "@/domain/util/utilities";
import { auth } from "@clerk/nextjs";
import { UpdateGroupForm } from "./components/update-group-form";

interface UpdateGroupPageProps {
  params: {
    groupId: string;
  };
}

const UpdateGroupPage = async ({ params }: UpdateGroupPageProps) => {
  const authentication = await auth();

  if (!authentication?.userId || !authentication.orgId) {
    return;
  }

  const groupService = new GroupService();
  const group = await groupService.findGroupById(
    authentication.orgId,
    authentication.userId,
    params.groupId
  );
  if (!group) {
    return;
  }

  const groupUserIds = group.users?.map((user) => user.userId) || [];
  const clerkService = new ClerkService();
  const groupUsers = await clerkService.getUsersById(groupUserIds);

  const groupUsersArray: { id: string; email: string }[] = [];

  groupUsers.forEach((user) => {
    const email = Utilities.getUserPrimaryEmailAddress(user);
    if (email) {
      groupUsersArray.push({
        id: user.id,
        email: email,
      });
    }
  });

  return <UpdateGroupForm group={group} groupUsers={groupUsersArray} />;
};

export default UpdateGroupPage;
