-- Change column from Quality to score
IF EXISTS(

  SELECT * FROM information_schema.columns
                  WHERE table_name = 'fames_batch_match'
                    AND table_schema = 'fa'
                    AND column_name = 'quality'

  )
THEN

-- CREATE NEW COLUMN
ALTER TABLE fames_batch_match ADD score DOUBLE DEFAULT 0 AFTER operator_id
;

-- MIGRATE old column
UPDATE fames_batch_match
   SET score = 100 -
       (CASE
        WHEN quality  = "Identical" THEN 0
        WHEN quality  = "Excellent" THEN 1
        WHEN quality  = "Good" THEN 3
        WHEN quality  = "Fair" THEN 7
        WHEN quality  = "Close" THEN 14
        WHEN quality  = "Bad" THEN 100
        END)
WHERE quality is not null
;

-- DROP old column
ALTER TABLE fames_batch_match
DROP quality;

END IF;


-- IF FALSE

-- DELETE FROM fames_batch_match
-- where operator_id IS NUL
-- ; 

-- END IF
