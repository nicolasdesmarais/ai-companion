export enum SecuredResourceAccessLevel {
  INSTANCE = "instance",
  ORGANIZATION = "organization",
  SELF = "self",
}

export const rankedAccessLevels = [
  SecuredResourceAccessLevel.INSTANCE,
  SecuredResourceAccessLevel.ORGANIZATION,
  SecuredResourceAccessLevel.SELF,
];
