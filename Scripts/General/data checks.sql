----check tables on the DB
SELECT table_name 
FROM information_schema.tables 
WHERE table_schema = 'public';

--selecting from the specific table
SELECT * FROM public."fy24q1" LIMIT 100;



-- Find rows with nulls in a specific column
SELECT * FROM fy24q1 WHERE column_name IS NULL;

-- Find rows with nulls in any of several columns
SELECT * FROM fy24q1
WHERE column1 IS NULL OR column2 IS NULL OR column3 IS NULL;

-- Count of nulls per column (example in PostgreSQL)
SELECT
  COUNT(*) FILTER (WHERE column1 IS NULL) AS column1_nulls,
  COUNT(*) FILTER (WHERE column2 IS NULL) AS column2_nulls
FROM your_table;


