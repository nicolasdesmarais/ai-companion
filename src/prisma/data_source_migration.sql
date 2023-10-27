CREATE TABLE `data_sources` (
  `id` varchar(191) COLLATE utf8mb4_unicode_ci NOT NULL,
  `created_at` datetime(3) NOT NULL DEFAULT CURRENT_TIMESTAMP(3),
  `updated_at` datetime(3) NOT NULL DEFAULT CURRENT_TIMESTAMP(3),
  `lastIndexedAt` datetime(3) DEFAULT NULL,
  `name` varchar(191) COLLATE utf8mb4_unicode_ci NOT NULL,
  `org_id` varchar(191) COLLATE utf8mb4_unicode_ci NOT NULL,
  `owner_user_id` varchar(191) COLLATE utf8mb4_unicode_ci NOT NULL,
  `type` enum('WEB_URL','GOOGLE_DRIVE','FILE_UPLOAD') COLLATE utf8mb4_unicode_ci NOT NULL,
  `index_percentage` decimal(65,30) NOT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;


CREATE TABLE `data_source_knowledges` (
  `data_source_id` varchar(191) COLLATE utf8mb4_unicode_ci NOT NULL,
  `knowledge_id` varchar(191) COLLATE utf8mb4_unicode_ci NOT NULL,
  PRIMARY KEY (`data_source_id`,`knowledge_id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;


CREATE TABLE `ai_data_sources` (
  `ai_id` varchar(191) COLLATE utf8mb4_unicode_ci NOT NULL,
  `data_source_id` varchar(191) COLLATE utf8mb4_unicode_ci NOT NULL,
  PRIMARY KEY (`ai_id`,`data_source_id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

INSERT INTO `data_source_knowledges` (`data_source_id`, `knowledge_id`)
SELECT UUID(), id
from knowledge;

INSERT INTO `data_sources` (`id`,`created_at`, `updated_at`, `lastIndexedAt`, `name`, `org_id`, `owner_user_id`, `type`, `index_percentage`)
SELECT dsk.data_source_id,
       `created_at`,
       `updated_at`,
       `updated_at`,
       `name`,
       'org_2Wqza6cFY4qeIUxlTTqUwr5Z62I',
       `user_id`,
       IF (TYPE = 'URL',
                  'WEB_URL',
                  'FILE_UPLOAD'), 100
FROM knowledge k
inner join data_source_knowledges dsk on dsk.knowledge_id = k.id;


INSERT INTO `ai_data_sources`(`ai_id`, `data_source_id`)
SELECT kai.companion_id,  dsk.data_source_id
from knowledge_ais kai
inner join data_source_knowledges dsk on dsk.knowledge_id = kai.knowledge_id;
