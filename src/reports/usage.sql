-- # of tokens stored per organization
SELECT o.org_id,
       sum(dk.token_count) AS org_token_count,
       round((sum(dk.token_count) * 4) / 1000000, 2) AS megabytes
FROM
  (SELECT DISTINCT org_id
   FROM data_sources d) AS o
INNER JOIN
  (SELECT DISTINCT d.org_id,
                   k.id,
                   k.token_count
   FROM data_sources d
   INNER JOIN data_source_knowledges dsk ON dsk.data_source_id = d.id
   INNER JOIN knowledge k ON k.id = dsk.knowledge_id) AS dk ON dk.org_id = o.org_id
GROUP BY o.org_id
ORDER BY org_token_count DESC;

-- # of tokens stored per organization & user
SELECT o.org_id,
       o.owner_user_id,
       sum(dk.token_count) AS org_token_count,
       round((sum(dk.token_count) * 4) / 1000000, 2) AS megabytes
FROM
  (SELECT DISTINCT org_id,
                   owner_user_id
   FROM data_sources d) AS o
INNER JOIN
  (SELECT DISTINCT d.org_id,
                   d.owner_user_id,
                   k.id,
                   k.token_count
   FROM data_sources d
   INNER JOIN data_source_knowledges dsk ON dsk.data_source_id = d.id
   INNER JOIN knowledge k ON k.id = dsk.knowledge_id) AS dk ON dk.org_id = o.org_id
AND dk.owner_user_id = o.owner_user_id
GROUP BY o.org_id,
         owner_user_id
ORDER BY org_token_count DESC;


-- # of tokens stored per AI
SELECT ak.ai_id,
       ak.name,
       sum(ak.token_count) AS ai_token_count,
       round((sum(ak.token_count) * 4) / 1000000, 2) AS megabytes
FROM
  (SELECT DISTINCT ais.name,
                   ad.ai_id,
                   k.id,
                   k.token_count
   FROM ais ais
   INNER JOIN ai_data_sources ad ON ad.ai_id = ais.id
   INNER JOIN data_source_knowledges dsk ON dsk.data_source_id = ad.data_source_id
   INNER JOIN knowledge k ON k.id = dsk.knowledge_id) AS ak
GROUP BY ak.ai_id
ORDER BY ai_token_count DESC;

-- # of tokens in messages per organization
SELECT c.org_id,
       sum(json_extract(metadata, "$.tokensUsed")) AS token_count
FROM messages m
INNER JOIN chats c ON c.id = m.chat_id
GROUP BY c.org_id
ORDER BY token_count DESC;

-- # of tokens in messages per organization and user
SELECT c.org_id,
       c.user_id,
       sum(json_extract(metadata, "$.tokensUsed")) AS token_count
FROM messages m
INNER JOIN chats c ON c.id = m.chat_id
GROUP BY c.org_id,
         c.user_id
ORDER BY token_count DESC;


-- # of tokens in messages per AI
SELECT ais.id,
       ais.name,
       sum(json_extract(metadata, "$.tokensUsed")) AS token_count
FROM messages m
INNER JOIN chats c ON c.id = m.chat_id
INNER JOIN ais ON ais.id = c.ai_id
GROUP BY ais.id
ORDER BY token_count DESC;
