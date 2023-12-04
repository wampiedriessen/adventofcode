-- CREATE TABLE solution (
--     part varchar,
--     answer varchar
-- );

CREATE TEMPORARY TABLE cards (
    cardnum int,
    winning int[],
    mine int[],
    winning_numbers int,
    points int,
    extra_copies int
);

CREATE FUNCTION array_intersect(anyarray, anyarray)
  RETURNS anyarray
  language sql
as $FUNCTION$
    SELECT ARRAY(
        SELECT UNNEST($1)
        INTERSECT
        SELECT UNNEST($2)
    );
$FUNCTION$;

INSERT INTO cards (cardnum, winning, mine, winning_numbers, points, extra_copies) 
    SELECT
        split_part(split_part(scratchcard, ':', 1), ' ', -1)::int,
        array_remove(string_to_array(split_part(split_part(scratchcard, ':', 2), '|', 1), ' '), '')::int[],
        array_remove(string_to_array(split_part(split_part(scratchcard, ':', 2), '|', 2), ' '), '')::int[],
        0, 0, 0
    FROM input;

UPDATE cards SET winning_numbers = cardinality( array_intersect(winning, mine) );
UPDATE cards SET points = POWER(2, winning_numbers - 1) WHERE winning_numbers > 0;

-- update cards c1 SET c1.extra_copies = (select sum(c2.extra_copies) from cards c2 where (c2.cardnum + c2.winning_numbers) >= c1.cardnum);


INSERT INTO solution (part, answer) SELECT '1', sum(points) FROM cards;
INSERT INTO solution (part, answer) SELECT '2', sum(extra_copies+1) FROM cards;

