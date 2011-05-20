SELECT *, pg_size_pretty(pg_relation_size(indexrelid))
 FROM pg_stat_all_indexes 
 ORDER BY pg_relation_size(indexrelid) DESC, idx_scan ASC

/*SELECT
    t.tablename,
    indexname,
    c.reltuples AS num_rows,
    pg_size_pretty(pg_relation_size(t.schemaname || '.' || t.tablename)) AS table_size,
    pg_size_pretty(pg_relation_size(indexrelid)) AS index_size,
    CASE WHEN x.is_unique = 1  THEN 'Y'
       ELSE 'N'
    END AS UNIQUE,
    idx_scan AS number_of_scans,
    idx_tup_read AS tuples_read,
    idx_tup_fetch AS tuples_fetched,
    foo.indexschemaname
FROM pg_tables t
LEFT OUTER JOIN pg_class c ON t.tablename=c.relname
LEFT OUTER JOIN
       (SELECT indrelid,
           max(CAST(indisunique AS integer)) AS is_unique
       FROM pg_index
       GROUP BY indrelid) x
       ON c.oid = x.indrelid
LEFT OUTER JOIN
    ( SELECT c.relname AS ctablename, ipg.relname AS indexname, x.indnatts AS number_of_columns, idx_scan, idx_tup_read, idx_tup_fetch,ins.relname as indexschemaname, indexrelname,x.indexrelid FROM pg_index x
           JOIN pg_class c ON c.oid = x.indrelid
           JOIN pg_class ipg ON ipg.oid = x.indexrelid
           JOIN pg_class ins ON c.relnamespace = ins.oid
           JOIN pg_stat_all_indexes psai ON x.indexrelid = psai.indexrelid )
    AS foo
    ON t.tablename = foo.ctablename
ORDER BY pg_relation_size(indexrelid) DESC;*/