import { GroupService } from "@/domain/services/GroupService";
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

  const groupUsers = (group.users ?? []).map((user) => ({
    id: user.userId,
    email: user.email,
  }));

  return (
    <UpdateGroupForm
      userId={authentication.userId}
      group={group}
      groupUsers={groupUsers}
    />
  );
};

export default UpdateGroupPage;
