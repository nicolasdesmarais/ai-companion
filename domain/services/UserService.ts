import { UserEntity } from "../entities/UserEntity";
import prismadb from "@/lib/prismadb";

export class UserService {

    public async findUserById(id: string) {
        return prismadb.user.findFirst({
            where: {
                id: id
            }
        });
    }

    public async findUserByExternalId(externalId: string) {
        return prismadb.user.findFirst({
            where: {
                externalId: externalId
            }
        });
    }

    public async create(user: UserEntity){
        return prismadb.user.create({data: user});
    }
}