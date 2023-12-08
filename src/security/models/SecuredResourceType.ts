export enum SecuredResourceType {
  AI = "ai",
  CHATS = "chats",
  DATA_SOURCES = "data-sources",
  GROUPS = "groups",
  ORG_CLIENT_CREDENTIALS = "org-client-credentials",
  ORG_SETTINGS = "org-settings",
}

export const availableApiResourceTypes = [
  SecuredResourceType.CHATS,
  SecuredResourceType.DATA_SOURCES,
];

export const orgOnlyResourceTypes = [
  SecuredResourceType.ORG_CLIENT_CREDENTIALS,
  SecuredResourceType.ORG_SETTINGS,
];
