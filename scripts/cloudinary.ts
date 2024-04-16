import prismadb from "@/src/lib/prismadb";

const cloudinary = require("cloudinary");

async function main() {
  try {
    const ais = await prismadb.aI.findMany();
    console.log(`Migrating images from ${ais.length} AIs ...`);
    for (const ai of ais) {
      if (ai.src) {
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
      }
    }
  } catch (error) {
    console.error("Error migrating cloudinary:", error);
  } finally {
    await prismadb.$disconnect();
  }
}

main();
