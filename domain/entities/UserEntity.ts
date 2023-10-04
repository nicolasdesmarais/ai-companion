export interface UserEntity {
    id?: string;
    name?: string;
    email: string;
    externalId?: string | null;
}