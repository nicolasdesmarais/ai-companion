const { PrismaClient } = require('@prisma/client');

const db = new PrismaClient();

async function main() {
  try {
    await db.category.createMany({
      data: [
        { name: 'Professional Coaches' },
        { name: 'SaaS' },
        { name: 'IaaS' },
        { name: 'Telco' },
        { name: 'Security' },
        { name: 'Mobility' },
        { name: 'Energy' },
      ],
    });
  } catch (error) {
    console.error('Error seeding default categories:', error);
  } finally {
    await db.$disconnect();
  }
}

main();