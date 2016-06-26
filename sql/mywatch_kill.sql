DELIMITER $$

DROP PROCEDURE IF EXISTS mysql.mywatch_kill $$
CREATE PROCEDURE mysql.mywatch_kill (IN i BIGINT)
  COMMENT 'Kill a query found in information_schema.processlist by ID'
-- It seems reasonable that this procedure kills the connection, not just
-- the query, because of the `DELETE` HTTP method on the process id. If the
-- connection is not killed, the process id remains.
BEGIN

  DECLARE n BIGINT;

  SELECT id INTO n
    FROM information_schema.processlist
    WHERE info IS NOT NULL
    AND host <> '' -- means non-system user
    AND id = i;

  IF (n IS NOT NULL) THEN
    KILL n; -- Use `CALL mysql.rds_kill(n);` on RDS
  END IF;

END $$

DELIMITER ;

