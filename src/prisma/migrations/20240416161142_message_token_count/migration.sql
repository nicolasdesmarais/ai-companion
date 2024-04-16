-- AlterTable
ALTER TABLE `messages` ADD COLUMN `token_count` INTEGER NULL;

-- Populate token_count
UPDATE messages
SET token_count = ifnull(json_extract(metadata, "$.tokensUsed"), 0) + ifnull(json_extract(metadata, "$.knowledgeTokensReturned"), 0);
