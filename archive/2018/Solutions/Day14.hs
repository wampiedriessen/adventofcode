{-# LANGUAGE ViewPatterns, PatternSynonyms #-}

module Solutions.Day14
( solvers
) where

import CommonHelpers
import qualified Data.Sequence as S

pattern Empty   <- (S.viewl -> S.EmptyL)  where Empty = S.empty
pattern x :< xs <- (S.viewl -> x S.:< xs) where (:<)  = (S.<|)
pattern xs :> x <- (S.viewr -> xs S.:> x) where (:>)  = (S.|>)

solvers = [solveP1,solveP2]

createRecipies :: Int -> Int -> S.Seq Int
createRecipies a b =
    let x = a+b
    in if x > 9
        then S.fromList [quot x 10, mod x 10]
        else S.fromList [x]

doPredicate pred newRec Empty r1 r2 =
    let a = mod (r1+1) $ length newRec
        b = mod (r2+1) $ length newRec
    in solveFor pred newRec a b
doPredicate pred old (x :< xs) r1 r2 =
    let new = old S.|> x
    in if pred new then new
        else doPredicate pred new xs r1 r2

solveFor :: (S.Seq Int -> Bool) -> S.Seq Int -> Int -> Int -> S.Seq Int
solveFor predicate recp a b =
    let r1 = recp `S.index` a
        r2 = recp `S.index` b
        created = createRecipies r1 r2
    in doPredicate predicate recp created (a+r1) (b+r2)

initSolve :: (S.Seq Int -> Bool) -> S.Seq Int
initSolve predicate = solveFor predicate (S.fromList [3,7]) 0 1

showSeq :: (S.Seq Int) -> String
showSeq = foldl (\a x -> a ++ (show x)) ""

solveP1 :: [String] -> String
solveP1 x = let
        target = read $ head x
        predicate = (> (target+10)) . S.length
        cut = S.take 10 . S.drop target
    in showSeq $ cut $ initSolve predicate

-- || Start Part 2

takeRight :: Int -> S.Seq a -> S.Seq a
takeRight i Empty = S.empty
takeRight i (rseq :> x) =
    if i <= 0 then S.empty
    else (takeRight (i-1) rseq) S.|> x

partCompare :: S.Seq Int -> S.Seq Int -> Bool
partCompare la = (la ==) . takeRight (S.length la)

createPredicate2 :: String -> S.Seq Int -> Bool
createPredicate2 = partCompare . S.fromList . map (read . (:""))

solveP2 :: [String] -> String
solveP2 x =
    let pred = createPredicate2 $ head x
        getLength s = (S.length s) - (length $ head x)
    in show $ getLength $ initSolve pred