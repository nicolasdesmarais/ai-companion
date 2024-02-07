export enum SecuredResourceType {
  AI = "ai",
  CHATS = "chats",
  DATA_SOURCES = "data-sources",
  GROUPS = "groups",
  ORG_CLIENT_CREDENTIALS = "org-client-credentials",
  ORG_SETTINGS = "org-settings",
  ORG_SUBSCRIPTIONS = "org-subscriptions",
  ORG_USAGE = "org-usage",
}

export const availableApiResourceTypes = [
  SecuredResourceType.AI,
  SecuredResourceType.CHATS,
  SecuredResourceType.DATA_SOURCES,
  SecuredResourceType.ORG_USAGE,
];

export const orgOnlyResourceTypes = [
  SecuredResourceType.ORG_CLIENT_CREDENTIALS,
  SecuredResourceType.ORG_SETTINGS,
];
