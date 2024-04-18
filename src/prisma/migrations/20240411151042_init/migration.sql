-- CreateTable
CREATE TABLE `categories` (
    `id` VARCHAR(191) NOT NULL,
    `name` VARCHAR(191) NOT NULL,

    PRIMARY KEY (`id`)
) DEFAULT CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;

-- CreateTable
CREATE TABLE `ais` (
    `id` VARCHAR(191) NOT NULL,
    `org_id` VARCHAR(191) NOT NULL,
    `user_id` VARCHAR(191) NOT NULL,
    `user_name` VARCHAR(191) NOT NULL,
    `src` TEXT NOT NULL,
    `name` TEXT NOT NULL,
    `introduction` TEXT NULL,
    `description` TEXT NOT NULL,
    `instructions` TEXT NOT NULL,
    `seed` TEXT NOT NULL,
    `visibility` ENUM('PRIVATE', 'GROUP', 'ORGANIZATION', 'ANYONE_WITH_LINK') NOT NULL DEFAULT 'PRIVATE',
    `list_in_org_catalog` BOOLEAN NOT NULL DEFAULT false,
    `list_in_public_catalog` BOOLEAN NOT NULL DEFAULT false,
    `options` JSON NULL,
    `profile` JSON NULL,
    `created_at` DATETIME(3) NOT NULL DEFAULT CURRENT_TIMESTAMP(3),
    `updated_at` DATETIME(3) NOT NULL,
    `category_id` VARCHAR(191) NOT NULL,
    `model_id` VARCHAR(191) NOT NULL DEFAULT 'llama2-13b',
    `external_id` VARCHAR(191) NULL,

    INDEX `ais_category_id_idx`(`category_id`),
    INDEX `ais_visibility_idx`(`visibility`),
    INDEX `ais_user_id_idx`(`user_id`),
    FULLTEXT INDEX `ais_name_user_name_idx`(`name`, `user_name`),
    PRIMARY KEY (`id`)
) DEFAULT CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;

-- CreateTable
CREATE TABLE `messages` (
    `id` VARCHAR(191) NOT NULL,
    `role` ENUM('user', 'system') NOT NULL,
    `content` TEXT NOT NULL,
    `created_at` DATETIME(3) NOT NULL DEFAULT CURRENT_TIMESTAMP(3),
    `updated_at` DATETIME(3) NOT NULL,
    `user_id` VARCHAR(191) NOT NULL,
    `metadata` JSON NULL,
    `chat_id` VARCHAR(191) NULL,

    INDEX `messages_chat_id_idx`(`chat_id`),
    PRIMARY KEY (`id`)
) DEFAULT CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;

-- CreateTable
CREATE TABLE `groups` (
    `id` VARCHAR(191) NOT NULL,
    `created_at` DATETIME(3) NOT NULL DEFAULT CURRENT_TIMESTAMP(3),
    `updated_at` DATETIME(3) NOT NULL DEFAULT CURRENT_TIMESTAMP(3),
    `org_id` VARCHAR(191) NOT NULL,
    `owner_user_id` VARCHAR(191) NOT NULL,
    `name` VARCHAR(191) NOT NULL,
    `availability` ENUM('EVERYONE', 'RESTRICTED') NOT NULL DEFAULT 'EVERYONE',

    PRIMARY KEY (`id`)
) DEFAULT CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;

-- CreateTable
CREATE TABLE `group_users` (
    `id` INTEGER NOT NULL AUTO_INCREMENT,
    `group_id` VARCHAR(191) NOT NULL,
    `user_id` VARCHAR(191) NULL,
    `email` VARCHAR(191) NOT NULL,
    `created_at` DATETIME(3) NOT NULL DEFAULT CURRENT_TIMESTAMP(3),
    `updated_at` DATETIME(3) NOT NULL DEFAULT CURRENT_TIMESTAMP(3),

    UNIQUE INDEX `group_users_group_id_email_key`(`group_id`, `email`),
    PRIMARY KEY (`id`)
) DEFAULT CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;

-- CreateTable
CREATE TABLE `group_ais` (
    `group_id` VARCHAR(191) NOT NULL,
    `ai_id` VARCHAR(191) NOT NULL,
    `created_at` DATETIME(3) NOT NULL DEFAULT CURRENT_TIMESTAMP(3),
    `updated_at` DATETIME(3) NOT NULL DEFAULT CURRENT_TIMESTAMP(3),

    PRIMARY KEY (`group_id`, `ai_id`)
) DEFAULT CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;

-- CreateTable
CREATE TABLE `knowledge` (
    `id` VARCHAR(191) NOT NULL,
    `created_at` DATETIME(3) NOT NULL DEFAULT CURRENT_TIMESTAMP(3),
    `updated_at` DATETIME(3) NOT NULL DEFAULT CURRENT_TIMESTAMP(3),
    `last_indexed_at` DATETIME(3) NULL,
    `name` TEXT NOT NULL,
    `type` ENUM('WEB_URL', 'GOOGLE_DRIVE', 'ONEDRIVE', 'FILE_UPLOAD', 'API') NOT NULL,
    `unique_id` VARCHAR(1000) NULL,
    `parent_unique_id` VARCHAR(1000) NULL,
    `index_status` ENUM('INITIALIZED', 'RETRIEVING_CONTENT', 'CONTENT_RETRIEVED', 'DOCUMENTS_CREATED', 'INDEXING', 'PARTIALLY_COMPLETED', 'COMPLETED', 'FAILED', 'DELETED') NOT NULL,
    `original_content` JSON NULL,
    `documents_blob_url` TEXT NULL,
    `metadata` JSON NULL,
    `document_count` INTEGER NULL,
    `token_count` INTEGER NULL,
    `is_migrated` BOOLEAN NOT NULL DEFAULT false,
    `index_percentage` DECIMAL(65, 30) NOT NULL DEFAULT 0,
    `is_blob_storage_deleted` BOOLEAN NOT NULL DEFAULT false,
    `is_vector_storage_deleted` BOOLEAN NOT NULL DEFAULT false,

    INDEX `knowledge_unique_id_idx`(`unique_id`(100)),
    INDEX `knowledge_parent_unique_id_idx`(`parent_unique_id`(100)),
    INDEX `knowledge_index_status_is_vector_storage_deleted_idx`(`index_status`, `is_vector_storage_deleted`),
    INDEX `knowledge_index_status_is_blob_storage_deleted_idx`(`index_status`, `is_blob_storage_deleted`),
    INDEX `knowledge_type_unique_id_index_status_idx`(`type`, `unique_id`(100), `index_status`),
    PRIMARY KEY (`id`)
) DEFAULT CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;

-- CreateTable
CREATE TABLE `knowledge_chunks` (
    `id` INTEGER NOT NULL AUTO_INCREMENT,
    `knowledge_id` VARCHAR(191) NOT NULL,
    `chunk_number` INTEGER NOT NULL,
    `start_index` INTEGER NULL,
    `end_index` INTEGER NULL,
    `status` ENUM('INDEXING', 'COMPLETED', 'FAILED') NOT NULL,
    `event_id` VARCHAR(191) NULL,
    `error` TEXT NULL,

    INDEX `knowledge_chunks_knowledge_id_status_idx`(`knowledge_id`, `status`),
    UNIQUE INDEX `knowledge_chunks_knowledge_id_chunk_number_key`(`knowledge_id`, `chunk_number`),
    PRIMARY KEY (`id`)
) DEFAULT CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;

-- CreateTable
CREATE TABLE `ai_permissions` (
    `id` INTEGER NOT NULL AUTO_INCREMENT,
    `user_id` VARCHAR(191) NULL,
    `ai_id` VARCHAR(191) NOT NULL,
    `email` VARCHAR(191) NOT NULL,

    INDEX `ai_permissions_user_id_idx`(`user_id`),
    UNIQUE INDEX `ai_permissions_ai_id_email_key`(`ai_id`, `email`),
    PRIMARY KEY (`id`)
) DEFAULT CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;

-- CreateTable
CREATE TABLE `oauth_tokens` (
    `id` VARCHAR(191) NOT NULL,
    `provider` ENUM('GOOGLE', 'MSFT') NOT NULL,
    `user_id` VARCHAR(191) NOT NULL,
    `email` VARCHAR(191) NOT NULL,
    `data` BLOB NULL,
    `created_at` DATETIME(3) NOT NULL DEFAULT CURRENT_TIMESTAMP(3),
    `updated_at` DATETIME(3) NOT NULL DEFAULT CURRENT_TIMESTAMP(3),

    UNIQUE INDEX `oauth_tokens_provider_user_id_email_key`(`provider`, `user_id`, `email`),
    PRIMARY KEY (`id`)
) DEFAULT CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;

-- CreateTable
CREATE TABLE `org_client_credentials` (
    `id` VARCHAR(191) NOT NULL,
    `provider` ENUM('GOOGLE', 'MSFT') NOT NULL,
    `org_id` VARCHAR(191) NOT NULL,
    `data` BLOB NULL,
    `created_at` DATETIME(3) NOT NULL DEFAULT CURRENT_TIMESTAMP(3),
    `updated_at` DATETIME(3) NOT NULL DEFAULT CURRENT_TIMESTAMP(3),

    UNIQUE INDEX `org_client_credentials_org_id_provider_key`(`org_id`, `provider`),
    PRIMARY KEY (`id`)
) DEFAULT CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;

-- CreateTable
CREATE TABLE `chats` (
    `id` VARCHAR(191) NOT NULL,
    `created_at` DATETIME(3) NOT NULL DEFAULT CURRENT_TIMESTAMP(3),
    `updated_at` DATETIME(3) NOT NULL DEFAULT CURRENT_TIMESTAMP(3),
    `messaged_at` DATETIME(3) NULL,
    `name` VARCHAR(191) NOT NULL,
    `pin_position` INTEGER NULL,
    `is_deleted` BOOLEAN NOT NULL DEFAULT false,
    `org_id` VARCHAR(191) NOT NULL,
    `user_id` VARCHAR(191) NOT NULL,
    `external_id` VARCHAR(191) NULL,
    `summary` TEXT NULL,
    `ai_id` VARCHAR(191) NOT NULL,

    INDEX `chats_user_id_idx`(`user_id`),
    PRIMARY KEY (`id`)
) DEFAULT CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;

-- CreateTable
CREATE TABLE `data_sources` (
    `id` VARCHAR(191) NOT NULL,
    `created_at` DATETIME(3) NOT NULL DEFAULT CURRENT_TIMESTAMP(3),
    `updated_at` DATETIME(3) NOT NULL DEFAULT CURRENT_TIMESTAMP(3),
    `last_indexed_at` DATETIME(3) NULL,
    `name` VARCHAR(500) NOT NULL,
    `org_id` VARCHAR(191) NOT NULL,
    `owner_user_id` VARCHAR(191) NOT NULL,
    `type` ENUM('WEB_URL', 'GOOGLE_DRIVE', 'ONEDRIVE', 'FILE_UPLOAD', 'API') NOT NULL,
    `index_status` ENUM('INITIALIZED', 'INDEXING', 'REFRESHING', 'PARTIALLY_COMPLETED', 'COMPLETED', 'FAILED', 'DELETION_REQUESTED', 'DELETED', 'MISSING') NULL,
    `index_percentage` DECIMAL(65, 30) NOT NULL,
    `document_count` INTEGER NULL,
    `token_count` INTEGER NULL,
    `data` JSON NULL,
    `refresh_period` ENUM('NEVER', 'DAILY', 'WEEKLY', 'MONTHLY') NULL,

    INDEX `data_sources_index_status_idx`(`index_status`),
    INDEX `data_sources_type_idx`(`type`),
    INDEX `data_sources_created_at_idx`(`created_at`),
    INDEX `data_sources_last_indexed_at_idx`(`last_indexed_at`),
    INDEX `data_sources_org_id_owner_user_id_idx`(`org_id`, `owner_user_id`),
    FULLTEXT INDEX `data_sources_name_idx`(`name`),
    PRIMARY KEY (`id`)
) DEFAULT CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;

-- CreateTable
CREATE TABLE `data_source_knowledges` (
    `data_source_id` VARCHAR(191) NOT NULL,
    `knowledge_id` VARCHAR(191) NOT NULL,

    INDEX `data_source_knowledges_knowledge_id_idx`(`knowledge_id`),
    PRIMARY KEY (`data_source_id`, `knowledge_id`)
) DEFAULT CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;

-- CreateTable
CREATE TABLE `ai_data_sources` (
    `ai_id` VARCHAR(191) NOT NULL,
    `data_source_id` VARCHAR(191) NOT NULL,

    INDEX `ai_data_sources_data_source_id_idx`(`data_source_id`),
    PRIMARY KEY (`ai_id`, `data_source_id`)
) DEFAULT CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;

-- CreateTable
CREATE TABLE `api_keys` (
    `id` VARCHAR(191) NOT NULL,
    `created_at` DATETIME(3) NOT NULL DEFAULT CURRENT_TIMESTAMP(3),
    `updated_at` DATETIME(3) NOT NULL,
    `last_used_at` DATETIME(3) NULL,
    `org_id` VARCHAR(191) NOT NULL,
    `user_id` VARCHAR(191) NOT NULL,
    `name` VARCHAR(191) NOT NULL,
    `key` VARCHAR(191) NOT NULL,
    `scopes` JSON NULL,

    UNIQUE INDEX `api_keys_key_key`(`key`),
    PRIMARY KEY (`id`)
) DEFAULT CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;

-- CreateTable
CREATE TABLE `ai_ratings` (
    `id` INTEGER NOT NULL AUTO_INCREMENT,
    `user_id` VARCHAR(191) NULL,
    `created_at` DATETIME(3) NOT NULL DEFAULT CURRENT_TIMESTAMP(3),
    `updated_at` DATETIME(3) NOT NULL DEFAULT CURRENT_TIMESTAMP(3),
    `ai_id` VARCHAR(191) NOT NULL,
    `review` TEXT NOT NULL,
    `headline` VARCHAR(191) NULL,
    `rating` INTEGER NOT NULL,

    PRIMARY KEY (`id`)
) DEFAULT CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;

-- CreateTable
CREATE TABLE `wait_list` (
    `id` INTEGER NOT NULL AUTO_INCREMENT,
    `created_at` DATETIME(3) NOT NULL DEFAULT CURRENT_TIMESTAMP(3),
    `updated_at` DATETIME(3) NOT NULL DEFAULT CURRENT_TIMESTAMP(3),
    `name` VARCHAR(191) NOT NULL,
    `email` VARCHAR(191) NOT NULL,
    `company` VARCHAR(191) NOT NULL,

    UNIQUE INDEX `wait_list_email_key`(`email`),
    PRIMARY KEY (`id`)
) DEFAULT CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;

-- CreateTable
CREATE TABLE `ai_org_approvals` (
    `id` INTEGER NOT NULL AUTO_INCREMENT,
    `created_at` DATETIME(3) NOT NULL DEFAULT CURRENT_TIMESTAMP(3),
    `updated_at` DATETIME(3) NOT NULL DEFAULT CURRENT_TIMESTAMP(3),
    `org_id` VARCHAR(191) NOT NULL,
    `ai_id` VARCHAR(191) NOT NULL,

    UNIQUE INDEX `ai_org_approvals_ai_id_org_id_key`(`ai_id`, `org_id`),
    PRIMARY KEY (`id`)
) DEFAULT CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;

-- CreateTable
CREATE TABLE `org_subscriptions` (
    `id` INTEGER NOT NULL AUTO_INCREMENT,
    `created_at` DATETIME(3) NOT NULL DEFAULT CURRENT_TIMESTAMP(3),
    `updated_at` DATETIME(3) NOT NULL DEFAULT CURRENT_TIMESTAMP(3),
    `org_id` VARCHAR(191) NOT NULL,
    `type` ENUM('FREE', 'PAID') NOT NULL DEFAULT 'FREE',
    `status` ENUM('ACTIVE', 'CANCELLED') NOT NULL DEFAULT 'ACTIVE',
    `period_end_date` DATETIME(3) NULL,
    `external_subscription_id` VARCHAR(191) NULL,
    `external_customer_id` VARCHAR(191) NULL,
    `data_usage_limit_gb` DOUBLE NULL,
    `api_usage_token_limit` INTEGER NULL,
    `metadata` JSON NULL,

    UNIQUE INDEX `org_subscriptions_org_id_key`(`org_id`),
    UNIQUE INDEX `org_subscriptions_external_subscription_id_key`(`external_subscription_id`),
    PRIMARY KEY (`id`)
) DEFAULT CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;

-- CreateTable
CREATE TABLE `ai_category_types` (
    `ai_id` VARCHAR(191) NOT NULL,
    `category_type` ENUM('AI_MODELS', 'PRODUCTIVITY', 'LEARNING_DEVELOPMENT', 'MARKETING', 'SALES', 'INFORMATION_TECHNOLOGY', 'ENGINEERING', 'HUMAN_RESOURCES', 'ACCOUNTING_FINANCE') NOT NULL,

    PRIMARY KEY (`ai_id`, `category_type`)
) DEFAULT CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;
