INSERT INTO `ais` (`id`, `org_id`, `user_id`, `user_name`, `src`, `name`, `description`, `instructions`, `seed`, `visibility`, `options`, `created_at`, `updated_at`, `category_id`, `model_id`)
select
`id`, `org_id`, `userId`, `userName`, `src`, `name`, `description`, `instructions`, `seed`, `visibility`, `options`, `created_at`, `updated_at`, `categoryId`, `modelId` From Companion;
