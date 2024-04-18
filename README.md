This is a [Next.js](https://nextjs.org/) project bootstrapped with [`create-next-app`](https://github.com/vercel/next.js/tree/canary/packages/create-next-app).

## Getting Started

First, run the development server:

```bash
npm run dev
# or
yarn dev
# or
pnpm dev
```

Open [http://localhost:3000](http://localhost:3000) with your browser to see the result.

You can start editing the page by modifying `app/page.tsx`. The page auto-updates as you edit the file.

This project uses [`next/font`](https://nextjs.org/docs/basic-features/font-optimization) to automatically optimize and load Inter, a custom Google Font.

## Learn More

To learn more about Next.js, take a look at the following resources:

- [Next.js Documentation](https://nextjs.org/docs) - learn about Next.js features and API.
- [Learn Next.js](https://nextjs.org/learn) - an interactive Next.js tutorial.

You can check out [the Next.js GitHub repository](https://github.com/vercel/next.js/) - your feedback and contributions are welcome!

## Deploy on Vercel

The easiest way to deploy your Next.js app is to use the [Vercel Platform](https://vercel.com/new?utm_medium=default-template&filter=next.js&utm_source=create-next-app&utm_campaign=create-next-app-readme) from the creators of Next.js.

Check out our [Next.js deployment documentation](https://nextjs.org/docs/deployment) for more details.

## Database Changes

Database changes are managed through [Prisma Migrations](https://www.prisma.io/docs/orm/prisma-migrate/getting-started). The process for making changes to the database is described below.

### Initial Setup

Prisma migrations require the use of a ["Shadow Database"](https://www.prisma.io/docs/orm/prisma-migrate/understanding-prisma-migrate/shadow-database#manually-configuring-the-shadow-database). This is a separate database that is used to detect problems such as schema drift or potential data loss of the generated migration.

By default Prisma attempts to create and drop the shadow DB automatically but that doesn't work when using one of our hosted DB instances as the DATABASE_URL. It's therefore necessary to setup a local database (e.g through running MySQL in a docker contain) and setting the `SHADOW_DATABASE_URL` environment variable to point to it.

To summarize:

1. Create a local MySQL database.
1. Add the following to your `.env` file: `SHADOW_DATABASE_URL='mysql://root:password@localhost:13306/prisma-shadow-db`

### Making Changes

1. Make changes to the Prisma schema in `prisma/schema.prisma`.
1. Generate a migration file:
   1. Run the `generate-db-migration` script,`npm run generate-db-migration --name="{migration-name}"`
   1. The above will generate a new folder and file under `prisma/migrations/`
   1. Make changes to the file if needed. In most cases the file will not needed any changes (e.g. when simply adding new columns or tables). Changes to the migration file might needed if we also need to run some additional SQL commands, like inserting or update data.
1. Commit and merge the migration file

The migration file will automatically be executed on any environment where the branch is deployed.

To deploy changes locally, run the `migrate-db` script, `npm run migrate-db`.

## Knowledge Ingestion

Knowledge ingestion is deeply asynchronous using Inngest events for queue orchestration.

### Local Inngest

Run the local Inngest client:

```zsh
npx inngest-cli@latest dev
```

### Local direct file upload

Direct file uploads use [Vercel Blob Client Uploads](https://vercel.com/docs/storage/vercel-blob/client-upload).

Make sure your `BLOB_READ_WRITE_TOKEN` variable is set and run `ngrok` to make your local instance reachable to vercel:

```zsh
ngrok http 3000
```

Now navigate to the external URL to upload the files, do not use `http://localhost:3000/`
