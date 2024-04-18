import { Permission } from "./Permission";

export interface AuthorizationContext {
  orgId: string;
  userId: string;
  type: AuthorizationContextType;
  permissions: Permission[];
}

export enum AuthorizationContextType {
  USER = "USER",
  API = "API",
}
