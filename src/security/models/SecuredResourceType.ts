export enum SecuredResourceType {
  CHATS = "chats",
  DATA_SOURCES = "data-sources",
  GROUPS = "groups",
  ORG_CLIENT_CREDENTIALS = "org-client-credentials",
}

export const availableApiResourceTypes = [
  SecuredResourceType.CHATS,
  SecuredResourceType.DATA_SOURCES,
];
