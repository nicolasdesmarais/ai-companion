import prismadb from "@/src/lib/prismadb";

export class CategoryService {
  public async getCategories() {
    return await prismadb.category.findMany();
  }
}

const categoryService = new CategoryService();
export default categoryService;
