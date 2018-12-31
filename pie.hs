{-
    Author: Beepbop0
    Desc: Given a list of ingredients, find the number of ingredients, find the maximum number of possible pies
    Link to original question: https://www.reddit.com/r/dailyprogrammer/comments/87rz8c/20180328_challenge_355_intermediate_possible/
-}
import System.IO
import Data.List
import Text.Read

-- recipes take the order of
-- pumpkin flavor
-- apples
-- eggs
-- cups of milk
-- cups of sugar
pumpkinRecipe :: [Int]
pumpkinRecipe   = [1, 0, 3, 4, 3]

appleRecipe :: [Int]
appleRecipe     = [0, 1, 4, 3, 2]

recipeLength :: Int
recipeLength    = length appleRecipe

main = do
    putStrLn "Given the following recipes:"
    putStrLn $ "pumpkin recipe:\t\t" ++ show pumpkinRecipe ++ "\napple recipe:\t\t" ++ show appleRecipe
    putStrLn "Enter a list of ingredients (in order of pumpkin flavoring, apples, eggs, cups of milk and sugar)"
    maybeRecp <- fmap readMaybe getLine :: IO (Maybe [Int])
    case maybeRecp of 
        Nothing -> main
        Just recp -> if length recp /= recipeLength
                        then main
                        else do
                    let maxTuple = maxTuples allTuples 
                        allTuples = allPieCombos recp
                    putStrLn "\nPie combinations are given in form (pumpkin pies, apple pies)"
                    putStrLn $ "All possible combination of pies for the given list of ingredients\n" ++ show allTuples
                    putStrLn $ "Max pies for given recipe\n" ++ show maxTuple
                

-- given a list of ingredients, find the max pair where fst = # apple pies and snd = # pumpkin pies
maxPies :: [Int] -> (Int, Int)
maxPies [] = error "provide a list of ingredients!"
maxPies i
    | length i /= recipeLength = error "wrong recipe list!"
    | otherwise = maxTuples $ allPieCombos i

-- maximize composability by using this as an accessible function
maxTuples :: [(Int, Int)] -> (Int, Int)
maxTuples = foldr1 (\(mP, mA) (tP, tA)
                                    -> if tP + tA > mP + mA 
                                          then (tP, tA) 
                                          else (mP, mA))

-- first find max number of pumpkins pies that can be made, then kick off recursive function
-- to find all corresponding pairs of pumpkin and apple pie combinations
allPieCombos :: [Int] -> [(Int, Int)]
allPieCombos i = 
               let il = zipWith (\x r -> x - (mp * r)) i pumpkinRecipe
                   mp = maxPiesForRecipe $ zip i pumpkinRecipe
               in  allPieCombos' mp il

-- given an initial max pumpkin pies amount, return a list of tuples that give the correspoding pairs of 
-- pumpkin and apple pies that can be made, until the number of pumpkin pies is 0
allPieCombos' :: Int -> [Int] -> [(Int, Int)]
allPieCombos' p i
    | p < 0 = []
    | otherwise =  
               let a = maxPiesForRecipe $ zip i appleRecipe
                   p' = p - 1 
                   newIngs = zipWith (+) i pumpkinRecipe  
               in  (p, a) : allPieCombos' p' newIngs

-- given a list of tuples, where fst entries correspond to provided ingredients, snd entries correspond to actual recipe
-- find the max number of pies for that given recipe
maxPiesForRecipe :: [(Int, Int)] -> Int
maxPiesForRecipe = foldr (\ (ing,recp) m 
                                    -> if recp == 0
                                        then m 
                                        else let m' = ing `quot` recp
                                              in if m' < m 
                                                    then m' 
                                                    else m) 
                    (maxBound :: Int) 
