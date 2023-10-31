export interface OAuthAdapter {
  validateToken(token: any): Promise<boolean>;
}
