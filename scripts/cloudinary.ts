const Prisma = require("@prisma/client");
const cloudinary = require("cloudinary");
const prismadb = new Prisma.PrismaClient();

async function migrateCloudinary() {
  try {
    const ais = await prismadb.aI.findMany();
    console.log(`Migrating images from ${ais.length} AIs ...`);
    for (const ai of ais) {
      if (ai.src && ai.src.includes("dew7iqxa4")) {
        try {
          console.log(
            `Migrating ${ai.name}'s image to the new cloudinary account...`
          );
          const data = await cloudinary.v2.uploader.unsigned_upload(
            ai.src,
            "bawmwjeq"
          );
          await prismadb.aI.update({
            where: { id: ai.id },
            data: {
              src: data.secure_url,
            },
          });
          console.log("Done.");
        } catch (error) {
          console.error("Error migrating cloudinary:", error);
        }
      }
    }
  } catch (error) {
    console.error("Error:", error);
  } finally {
    await prismadb.$disconnect();
  }
}

migrateCloudinary();
