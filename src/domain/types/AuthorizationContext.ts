export interface AuthorizationContext {
  orgId: string;
  userId: string;
  type: AuthorizationContextType;
}

export enum AuthorizationContextType {
  USER = "USER",
  API = "API",
}
