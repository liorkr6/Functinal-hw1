-- Implement the following functions.
-- When you're done, ghc -Wall -Werror HW1.hs should successfully compile.

-- Tells HLS to show warnings, and the file won't be compiled if there are any warnings, e.g.,
-- eval (-- >>>) won't work.
{-# OPTIONS_GHC -Wall -Werror #-}
-- Refines the above, allowing for unused imports.
{-# OPTIONS_GHC -Wno-unused-imports #-}


module HW1 where

import Prelude (Bool (..), Char, Either (..), Enum (..), Eq (..), Int, Maybe (..), Num (..), Ord (..), Show (..), String, all, and, any, concat, concatMap, const, curry, elem, error, filter, flip, foldl, foldr, fst, id, length, lookup, map, not, notElem, null, or, product, reverse, snd, sum, uncurry, undefined, (!!), ($), (&&), (++), (.), (||))

-- Section 1: Utility functions
-- Basic Maybes
fromMaybe :: a -> Maybe a -> a
fromMaybe x Nothing = x
fromMaybe _ (Just y) = y
-- >>> fromMaybe 1 Nothing
-- >>> fromMaybe 1 (Just 2)
-- 1
-- 2

maybe :: b -> (a -> b) -> Maybe a -> b
maybe x _ Nothing = x
maybe _ f (Just x) = f x
-- >>> maybe 1 length Nothing
-- >>> maybe 1 length (Just "foo")
-- 1
-- 3

catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes (Nothing : xs) = catMaybes xs
catMaybes (Just x : xs) = x : catMaybes xs
-- >>> catMaybes [Just 1, Nothing , Just 3]
-- [1,3]

mapMaybe :: (a -> Maybe b) -> [a] -> [b]
mapMaybe _ [] = []
mapMaybe f (x : xs) = go (f x) ++ mapMaybe f xs
    where
        go Nothing = []
        go (Just y) = [y]
-- >>> mapMaybe (\x -> if x > 0 then Just $ x * 10 else Nothing) [1, -1, 10]
-- [10,100]


-- Basic Eithers
either :: (a -> c) -> (b -> c) -> Either a b -> c
either f _ (Left x) = f x
either _ g (Right t) = g t
-- >>> either length (*10) $ Left "foo"
-- >>> either length (*10) $ Right 10
-- 3
-- 100

mapLeft :: (a -> c) -> Either a b -> Either c b
mapLeft f (Left x) = Left $ f x
mapLeft _ (Right y) = Right y
-- >>> mapLeft (++ "bar") (Left "foo")
-- >>> mapLeft (++ "bar") (Right 10)
-- Left "foobar"
-- Right 10


catEithers :: [Either a b] -> Either a [b]
catEithers [] = Right []
catEithers (Left x : _) = Left x
catEithers (Right x : xs) = case catEithers xs of
                               Left y -> Left y
                               Right ys -> Right (x : ys)
--  >>> catEithers [Right 10, Left "foo", Right 20, Left "bar"]
--  >>> catEithers [Right 10, Right 20]
--  >>> catEithers [Right 10, Right 20, Right 40, Right 30]
--  >>> catEithers [Right 10, Right 20, Right 40, Right 30, Left "error"]
-- Left "foo"
-- Right [10,20]
-- Right [10,20,40,30]
-- Left "error"

-- Left "foo"
-- Right [10,20]

mapEither :: (a -> Either b c) -> [a] -> Either b [c]
mapEither _ [] = Right []
mapEither f (x : xs) = case f x of
                        Left y -> Left y
                        Right y -> case mapEither f xs of
                                    Left z -> Left z
                                    Right ys -> Right (y : ys)

-- >>> mapEither (\x -> if x > 0 then Right $ x * 10 else Left $ x + 5) [1, 2, 3]
-- >>> mapEither (\x -> if x > 0 then Right $ x * 10 else Left $ x + 5) [1, -1, 2, -2]
-- Right [10,20,30]
-- Left 4

concatEitherMap :: (b -> Either a c) -> Either a b -> Either a c
concatEitherMap _ (Left a) = Left a
concatEitherMap f (Right b) = f b

-- >>> concatEitherMap (Right . (* 10)) (Right 5)
-- >>> concatEitherMap (Right . (* 10)) (Left 5)
-- Right 50
-- Left 5

partitionEithers :: [Either a b] -> ([a], [b])
partitionEithers [] = ([],[])
partitionEithers (Left x : xs) = let (as, bs) = partitionEithers xs
                                 in (x : as, bs)
partitionEithers (Right x : xs) = let (as, bs) = partitionEithers xs
                                  in (as, x : bs)

-- >>> partitionEithers [Right "foo", Left 42, Right "bar", Left 54]
-- ([42,54],["foo","bar"])

-- Section 2: Lists and zips
-- Fun with lists and zips
-- snoc is the opposite of cons, i.e., append to list.
snoc :: [a] -> a -> [a]
snoc [] x = [x]
snoc (y : ys) x = y : snoc ys x

-- >>> snoc [1,2,3] 4
-- [1,2,3,4]


-- -- If one list is shorter than the other, take the shorter one, e.g.,
-- zipWith (+) [1, 2] [3, 4, 5] returns [4, 6]
zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith _ _ [] = []
zipWith _ [] _ = []
zipWith f (x: xs) (y : ys) = f x y : zipWith f xs ys

-- >>> zipWith (+) [1, 2, 3] [4, 5, 6]
-- >>> zipWith (+) [1, 2] [4, 5, 6]
-- [5,7,9]
-- [5,7]

-- -- If one list is shorter than the other, take the shorter one, e.g.,
-- -- zip [1, 2] [3, 4, 5] returns [(1, 3), (2, 4)]
-- -- Could you implement this using zipWith and point-free style?
unzip :: [(a, b)] -> ([a], [b])
unzip [] = ([],[])
unzip ((x, y) : xs) = (x : l, y : r)
  where
    (l, r) = unzip xs

zip :: [a] -> [b] -> [(a, b)]
zip = zipWith (, )
-- >>> unzip [(1, 2), (3, 4)]
-- ([1,3],[2,4])
-- >>> zip [1, 2] [3, 4, 5]
-- [(1,3),(2,4)]


-- -- Section 3: String interpolation
-- -- Parsing template strings, e.g., "Hello ${name}!". See the PDF for more information.
splitOn :: Char -> String -> Maybe (String, String)
splitOn _ [] = Nothing
splitOn c (x : xs) = if c == x then Just ("" , xs) else case splitOn c xs of
    Just (head, tail) -> Just (x : head, tail)
    _ -> Nothing

-- >>> splitOn 'x' "foobar"
-- >>> splitOn 'x' "fooxbar"
-- >>> splitOn 'x' "foox"
-- >>> splitOn 'x' "fooxfooxfoo"
-- Nothing
-- Just ("foo","bar")
-- Just ("foo","")
-- Just ("foo","fooxfoo")


type Variable = String
data ParsedString = PlainString String | Variable String deriving Show
parseTemplate :: String -> Maybe [ParsedString]
parseTemplate "" = Just []
parseTemplate str = case splitOn '$' str of
  Nothing -> Just [PlainString str]
  Just ("", "") -> Nothing
  Just (head, "") -> Just [PlainString head]
  Just (head, h:tail) -> case h of
    '{' -> case splitOn '}' tail of
        Just ("", _) -> Nothing
        Nothing -> Nothing
        Just (var, tail2) -> case parseTemplate tail2 of
          Nothing -> Nothing
          Just res -> Just (PlainString head : Variable var : res)
    _ -> Nothing

-- >>> parseTemplate "Hello${world}!"
-- Just [PlainString "Hello",Variable "world",PlainString "!"]
-- >>> parseTemplate "Hello${!"
-- Nothing
-- >>> parseTemplate "Hello$!"
-- Nothing

type VariableName = String
type VariableValue = String
type MissingVariable = String
assignTemplate :: [(VariableName, VariableValue)] -> [ParsedString] -> Either MissingVariable String
assignTemplate _ [] = Right ""
assignTemplate values (Variable var : rest) = case lookup var values of
    Nothing -> Left var
    Just value -> case assignTemplate values rest of
        Left res -> Left res
        Right res -> Right (value ++ res)
assignTemplate values (PlainString str : rest) = case assignTemplate values rest of
    Left res -> Left res
    Right res -> Right (str ++ res)

-- >>> assignTemplate [] [ PlainString " Hello !"]
-- >>> parsed = [ PlainString " Hello ", Variable " name ", PlainString "!"]
-- >>> assignTemplate [(" name ", " Simon ")] parsed
-- >>> assignTemplate [(" Name ", " Simon ")] parsed
-- >>> assignTemplate [] [ Variable "x", Variable "y"]
-- Right " Hello !"
-- Right " Hello  Simon !"
-- Left " name "
-- Left "x"

data Error = MissingVar MissingVariable | InvalidTemplate deriving Show
interpolateString :: [(VariableName, VariableValue)] -> String -> Either Error String
interpolateString values template = case parseTemplate template of
    Nothing -> Left InvalidTemplate
    Just parsedTemplate -> case assignTemplate values parsedTemplate of
        Left var -> Left (MissingVar var)
        Right str -> Right str

-- >>> interpolateString [(" name ", " Simon ")] " Hello ${ name }!"
-- >>> interpolateString [(" name ", " Simon ")] " Hello $name !"
-- >>> interpolateString [(" Name ", " Simon ")] " Hello ${ name }!"
-- Right " Hello  Simon !"
-- Left InvalidTemplate
-- Left (MissingVar " name ")


-- Section 4: N-queens problem
-- Queens and helpers.
-- range of a non-positive number is empty, range 3 is [0, 1, 2]
range :: Int -> [Int]
range n = [0 .. n - 1]
-- >>> range 4
-- []

-- enumerate "foo" should return [(0, 'f'), (1, 'o'), (2, 'o')]
-- Hint: Use zip
enumerate :: [a] -> [(Int, a)]
enumerate = zip [0 ..]
-- >>> enumerate (range 4)


-- Splits [1, 2, 3] should return [([1, 2, 3],[]), ([1, 2], [3]), ([1], [2, 3]), ([], [1, 2, 3]).
-- Order is important!
-- Hint: Splits [] is [([], [])].
splits :: [a] -> [([a], [a])]
splits [] = [([], [])]
splits (head:tail) = [(head:x, y) | (x, y) <- rest] ++ [([], head:tail)]
    where rest = splits tail

-- >>> splits [1,2,3]
-- [([1,2,3],[]),([1,2],[3]),([1],[2,3]),([],[1,2,3])]
-- >>> splits [1,2]
-- [([1,2],[]),([1],[2]),([],[1,2])]

-- permutations of [] is [[]]
-- permutations of [1, 2, 3] is [[1, 2, 3], [1, 3, 2], [2, 3, 1], [2, 1, 3], [3, 1, 2], [3, 2, 1]]
-- Hint: use splits
-- -- order is not important
permutations :: [a] -> [[a]]
permutations [] = [[]]
permutations (head:tail) = concatMap (insertItem head) (permutations tail)
  where
    insertItem :: a -> [a] -> [[a]]
    insertItem item [] = [[item]]
    insertItem item (head2:tail2) = (item:head2:tail2) : map (head2:) (insertItem item tail2)

-- >>> permutations [1,3,4]
-- [[1,3,4],[3,1,4],[3,4,1],[1,4,3],[4,1,3],[4,3,1]]

type Column = Int
type Solution = [Column]
-- Returns all the solutions the n-queens problem. Returns a list of solutions, each solution made
-- up up of column per row. For example, queens 1 returns [[0]], queens 2 and queens 3 return [],
-- queens 4 returns [[1,3,0,2],[2,0,3,1]].
-- Order is not important.
queens :: Int -> [Solution]
queens n = filter checkValidSolution $ permutations (range n)
    where
        checkValidSolution ::Solution -> Bool
        checkValidSolution [] = True
        checkValidSolution solution = not (any checkPair ([(x, y) | x <- enumerate solution, y <- enumerate solution, x < y]))

        checkPair :: ((Int, Int), (Int, Int)) -> Bool
        checkPair ((col1,row1), (col2,row2)) = abs (col1 - col2) == abs (row1 - row2)

-- >>> queens 4
-- [[2,0,3,1],[1,3,0,2]]
