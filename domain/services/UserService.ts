import { UserEntity } from "../entities/UserEntity";
import prismadb from "@/lib/prismadb";

export class UserService {

    public async create(user: UserEntity){
        return prismadb.user.create({data: user});
    }
}