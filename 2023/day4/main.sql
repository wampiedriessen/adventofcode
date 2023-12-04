CREATE TEMPORARY TABLE scratchcards (
    cardnum int,
    winning int[],
    mine int[],
    number_won int default 0,
    points int default 0
);

INSERT INTO scratchcards (cardnum, winning, mine) 
    SELECT
        split_part(split_part(scratchcard, ':', 1), ' ', -1)::int,
        array_remove(string_to_array(split_part(split_part(scratchcard, ':', 2), '|', 1), ' '), '')::int[] as winning,
        array_remove(string_to_array(split_part(split_part(scratchcard, ':', 2), '|', 2), ' '), '')::int[] as mine
    FROM input;

update scratchcards set number_won = cardinality( ARRAY(select unnest(winning) INTERSECT select unnest(mine)) );
update scratchcards set points = CASE WHEN number_won > 0 THEN POWER(2, number_won - 1) ELSE 0 END;

INSERT INTO solution (part, answer) SELECT '1', sum(points) FROM scratchcards;

with recursive r_scratch as (
    select * from scratchcards
    UNION ALL
        select newcard.* from scratchcards newcard
            join r_scratch winner on newcard.cardnum <= (winner.cardnum + winner.number_won) and newcard.cardnum > winner.cardnum
) INSERT INTO solution (part, answer) SELECT '2', count(*) FROM r_scratch;
