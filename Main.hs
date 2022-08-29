--imports

import Data.List as L ( elemIndices )

import qualified Data.Text as T

import qualified Data.Set as S

import System.TimeIt ( timeIt )

--my libs

import Basic ( badImul', iterateFunc, naturalPos, replaceAtIndex, toUInt32 )

import Binary.Shift ( shiftBinL32, shiftBinR32 )

import Binary.Logicgates ( xorB' )

import Binary.Conversion ( decToBase, toDec )

--this works every time
range :: Int -> Int -> Int -> Int
range min max seed = nextUInt seed `mod` (max - min + 1) + min

nextUInt seed =
    toDec (xorB' x' y')
    where
        seedBin = decToBase 2 seed ""
        x     = xorB' seedBin $ shiftBinL32 11 seedBin --add if '1' then two else continue
        x'    = xorB' x       $ shiftBinR32 8 x
        y     = decToBase 2   (iterateFunc  3 badImul' seed) ""
        y'    = xorB' y       $ shiftBinR32 19 y

display :: [String]
display =        ["Golden Powder","Bombushka","Seer's Blood","Rook's Bomb","Lightning Bomb","Wayland's Boots","Galoshes","Bottled Lightning","Salamander Tail","Guidance","Ursine Ring","Guantes","Demon Ring","Hungry Ghost","Intensifier","Cracked Orb","Berserker's Pendant","Map","Four Leaf Clover","Conductor","Gold Tooth","Hoodie's Pillow","Grimhilde's Mirror","Meal Ticket","Shadow's Fang","Dillon's Claw","Bramble Vest","Leftovers","Spare Ordnance","Large Ember","Pilfer's Gift","Duplicator","Simple Chest","Unstable Concoction","Totem of Life","Golden Popcorn","Miner's Flask","Hyperstone","Bishop's Bomb","Sewing Kit","Floating Skull","Float Boots","Key Blade","War Paint","Sonic Boom","Gold Frenzy","Selt's Egg","Resurrection","Butcher's Cleaver","Iron Branch","Lava Walkers","Rat Bond","Master Pickaxe","Knight's Pendant","Queen's Crown","King's Crown","Aegis","Adventurer's Whip","Adventurer's Hat","Axe Thrower's Pendant","Archer's Pendant","Gordon's Tunic","Catalyst","Mediocre Ring","Crippling Poison","Cosmic Egg","Kurtz' Stache","Lucky Charm","Sequence Breaker","Inverter","Fan of Knives","Aphotic Charm","Battle Standard","Othermine Conduit","Battle Axe","Glaive","Twisted Blade","Tent","Doll","Masa","Lunchbox","Mune","Remote Detonator","Recycler","U-235","Electrified Orb","Throwing Star","108 Beads","Vorpal Blade","Breastplate","Dirk's Hammer","Blast Suit","Caustic Vial","Blood Bomb","Holy Guacamole","Phantasmal Axe","Paladin Shield","Gecko Blast","Soul Cannon","Gauntlets","Greaves","Pauldron","Obsidian Knife","Birthing Pod","Fork","Ara","Ursa Major","Canis Major","Sagitta","Circinus","Orion's Sword","Heavy Boots","Shrapnel","Magnetized Ore","Lucky Nugget","Twin Axe","Tortoise Shield","Javelin","Golden Axe","Bounty Contract"]

newsaveDisplay :: [String]
newsaveDisplay = ["Bombushka","Seer's Blood","Rook's Bomb","Lightning Bomb","Galoshes","Bottled Lightning","Salamander Tail","Guidance","Ursine Ring","Demon Ring","Intensifier","Cracked Orb","Conductor","Grimhilde's Mirror","Meal Ticket","Dillon's Claw","Bramble Vest","Leftovers","Spare Ordnance","Simple Chest","Unstable Concoction","Totem of Life","Golden Popcorn","Miner's Flask","Sewing Kit","Floating Skull","Float Boots","Key Blade","War Paint","Sonic Boom","Gold Frenzy","Butcher's Cleaver","Iron Branch","Knight's Pendant","Queen's Crown","Aegis","Adventurer's Whip","Axe Thrower's Pendant","Cosmic Egg","Battle Standard","Battle Axe","Tent","Masa","Lunchbox","Phantasmal Axe","Gecko Blast","Soul Cannon","Greaves","Pauldron","Obsidian Knife","Fork","Ursa Major","Canis Major","Sagitta","Circinus","Orion's Sword","Shrapnel","Tortoise Shield","Golden Axe"]

starterDisplay :: [String]
starterDisplay = ["Bottled Lightning","Butcher's Cleaver","Bombushka","Golden Popcorn","Guidance","Phantasmal Axe","Floating Skull","Salamander Tail","Fork"]

masterindex index = display !! index

items :: [Int]
items =        [9,9,9,9,9,9,9,9,9,9,9,9,9,3,9,3,9,3,3,9,9,3,9,9,9,9,9,9,9,3,3,3,9,9,9,9,9,3,9,0,3,9,9,9,9,9,3,3,9,9,9,3,3,9,3,3,9,3,9,9,9,9,9,9,9,3,9,9,9,2,9,9,9,3,9,9,9,9,9,3,9,3,3,9,3,3,9,9,9,5,3,9,9,9,2,9,2,1,3,5,5,5,3,3,3,9,9,9,9,9,9,9,9,9,3,3,9,9,3,9]

newsaveItems :: [Int]
newsaveItems = [9,9,9,9,9,9,9,9,9,9,9,3,9,9,9,9,9,9,9,9,9,9,9,9,3,3,9,9,9,9,9,9,9,9,3,9,3,9,3,9,9,9,3,9,9,1,3,5,5,3,3,9,9,9,9,9,9,9,3]

starterItems = [9,9,9,9,9,9,9,9,1]

remove :: Eq a => a -> [a] -> [a]
remove element = filter (/= element)

test23 :: [String]
test23 = remove "" $ zipWith (\ x y -> if y /= 0 then x else "")  display newsaveItems

test3 :: Int -> [Int] -> Int
test3 seed items = getIndex seed items (sum items)

getIndex seed items sum = reduceWeight items $ range 1 sum seed

test190 x x1 list
    | x == (-1) = list
    | otherwise = replaceAtIndex (head $ elemIndices (starterDisplay !! x) newsaveDisplay) 0 newsaveItems

replaceAtIndex' :: (Eq a1, Num a1, Num a2) => a1 -> Int -> [a2] -> [a2]
replaceAtIndex' x x2 list
    | x == (-1) = list
    | otherwise = replaceAtIndex x2 0 list

starterToNewsave y = case y of
     0 -> 5
     1 -> 31
     2 -> 0
     3 -> 22
     4 -> 7
     5 -> 44
     6 -> 25
     7 -> 6
     8 -> 50
     _ -> -1

-- |
-- Backtracking for new save
-- 
-- Note: if user input is -1, due to the laziness of haskell it won't calculate the relics, it'll just skip it
--
-- e.g | newsaveBacktracking [5, -1, -1, -1, -1, 6, 7, -1, -1, -1, -1, -1, -1, 8, -1, -1, -1, -1, -1, -1] 2 -> 
--
-- returns: [seed]
newsaveBacktracking :: [Int] -> Int -> [Int]
newsaveBacktracking list n = take n [seed |
    -- TODO: change -1 to a maybe
    let sum0     =  sum starterItems,

    --Seed
    seed <- [900000..99999999],

    --Tutorial
    let x1       =  getIndex (seed + 1) starterItems sum0, --find relic in starter relics
    [x1]         == take 1 list,

    --Mine 2
    let index1   = list !! 1,
    let items1   = replaceAtIndex (starterToNewsave x1) 0 newsaveItems, --replace last found relic to not show up again
    let sum1     = sum items1, --get the sum of items
    let x2       = getIndex (seed + 2) items1 sum1, --find relic specific relic
    x2 == index1 || index1 == (-1), --check if user doesn't want relic or relic is what user wants

    --Mine 3
    let index2   = list !! 2,
    let items2   = replaceAtIndex' index1 x2 items1, --replace last found relic to not show up again
    let sum2     = sum1 -  sum' index1 x2 items1,
    let x1       = getIndex (seed + 3) items2 sum2,
    x1 == index2 || index2 == (-1),

    --Mine 4
    let index1   = list !! 3,
    let items1   = replaceAtIndex' index2 x1 items2, --replace last found relic to not show up again
    let sum1     = sum2 - sum' index2 x1 items2,
    let x2       = getIndex (seed + 4) items1 sum1,
    x2 == index1 || index1 == (-1),

    --Dungeon 1
    let index2   = list !! 4,
    let items2   = replaceAtIndex' index1 x2 items1, --replace last found relic to not show up again
    let sum2     = sum1 - sum' index1 x2 items1,
    let x1       = getIndex (seed + 6) items2 sum2,
    x1 == index2 || index2 == (-1),

    --Dungeon 2
    let index1   = list !! 5,
    let items1   = replaceAtIndex' index2 x1 items2, --replace last found relic to not show up again
    let sum1     = sum2 - sum' index2 x1 items2,
    let x2       = getIndex (seed + 7) items1 sum1,
    x2 == index1 || index1 == (-1),

    --Dungeon 3
    let index2   = list !! 6,
    let items2   = replaceAtIndex' index1 x2 items1, --replace last found relic to not show up again
    let sum2     = sum1 - sum' index1 x2 items1,
    let x1       = getIndex (seed + 8) items2 sum2,
    x1 == index2 || index2 == (-1),

    --Dungeon 4
    let index1   = list !! 7,
    let items1   = replaceAtIndex' index2 x1 items2, --replace last found relic to not show up again
    let sum1     = sum2 - sum' index2 x1 items2,
    let x2       = getIndex (seed + 9) items1 sum1,
    x2 == index1 || index1 == (-1),

    --Halls 1
    let index2   = list !! 8,
    let items2   = replaceAtIndex' index1 x2 items1, --replace last found relic to not show up again
    let sum2     = sum1 - sum' index1 x2 items1,
    let x1       = getIndex (seed + 11) items2 sum2,
    x1 == index2 || index2 == (-1),

    --Halls 2
    let index1   = list !! 9,
    let items1   = replaceAtIndex' index2 x1 items2, --replace last found relic to not show up again
    let sum1     = sum2 - sum' index2 x1 items2,
    let x2       = getIndex (seed + 12) items1 sum1,
    x2 == index1 || index1 == (-1),

    --Halls 3
    let index2   = list !! 10,
    let items2   = replaceAtIndex' index1 x2 items1, --replace last found relic to not show up again
    let sum2     = sum1 - sum' index1 x2 items1,
    let x1       = getIndex (seed + 13) items2 sum2,
    x1 == index2 || index2 == (-1),

    --Halls 4
    let index1   = list !! 11,
    let items1   = replaceAtIndex' index2 x1 items2, --replace last found relic to not show up again
    let sum1     = sum2 - sum' index2 x1 items2,
    let x2       = getIndex (seed + 14) items1 sum1,
    x2 == index1 || index1 == (-1),

    --Caverns 1
    let index2   = list !! 12,
    let items2   = replaceAtIndex' index1 x2 items1, --replace last found relic to not show up again
    let sum2     = sum1 - sum' index1 x2 items1,
    let x1       = getIndex (seed + 16) items2 sum2,
    x1 == index2 || index2 == (-1),

    --Caverns 2
    let index1   = list !! 13,
    let items1   = replaceAtIndex' index2 x1 items2, --replace last found relic to not show up again
    let sum1     = sum2 - sum' index2 x1 items2,
    let x2       = getIndex (seed + 17) items1 sum1,
    x2 == index1 || index1 == (-1),

    --Caverns 3
    let index2   = list !! 14,
    let items2   = replaceAtIndex' index1 x2 items1, --replace last found relic to not show up again
    let sum2     = sum1 - sum' index1 x2 items1,
    let x1       = getIndex (seed + 18) items2 sum2,
    x1 == index2 || index2 == (-1),

    --Caverns 4
    let index1   = list !! 15,
    let items1   = replaceAtIndex' index2 x1 items2, --replace last found relic to not show up again
    let sum1     = sum2 - sum' index2 x1 items2,
    let x2       = getIndex (seed + 19) items1 sum1,
    x2 == index1 || index1 == (-1),

    --Core 1
    let index2   = list !! 16,
    let items2   = replaceAtIndex' index1 x2 items1, --replace last found relic to not show up again
    let sum2     = sum1 - sum' index1 x2 items1,
    let x1       = getIndex (seed + 21) items2 sum2,
    x1 == index2 || index2 == (-1),

    --Core 2
    let index1   = list !! 17,
    let items1   = replaceAtIndex' index2 x1 items2, --replace last found relic to not show up again
    let sum1     = sum2 - sum' index2 x1 items2,
    let x2       = getIndex (seed + 22) items1 sum1,
    x2 == index1 || index1 == (-1),

    --Core 3
    let index2   = list !! 18,
    let items2   = replaceAtIndex' index1 x2 items1, --replace last found relic to not show up again
    let sum2     = sum1 - sum' index1 x2 items1,
    let x1       = getIndex (seed + 23) items2 sum2,
    x1 == index2 || index2 == (-1),

    --Core 4
    let index1   = list !! 19,
    let items1   = replaceAtIndex' index2 x1 items2, --replace last found relic to not show up again
    let sum1     = sum2 - sum' index2 x1 items2,
    let x2       = getIndex (seed + 24) items1 sum1,
    x2 == index1 || index1 == (-1)


    ]


test5 :: [Int] -> [(Int)]
test5 list = take 1 [(seed) |
    let items1   = replaceAtIndex 105 0 items,
    seed <- [100000..99999999],

    let x1       = test3 (seed + 1) items1,
    [x1]         == take 1 list,

    let items2   = replaceAtIndex x1 0 items1,
    let x2       = test3 (seed + 2) items2,
    [x1, x2]     == take 2 list

    -- let items3   = replaceAtIndex x2 0 items2,
    -- let x3       = test3 (seed + 3) items3,
    -- [x1, x2, x3] == take 3 list 
    ]

sum' x y z
            | x == (-1) = 0
            | otherwise = z !! naturalPos y

test4 :: Int -> [Int] -> Int -> [Int]
test4 seed items n = do
    let seed' = seed + (if n `mod` 5 == 0 then 2 else 1)
    let x = reduceWeight items $ range 1 (sum items) seed'
    if n == 17
        then []
    else
        x : test4 seed' (replaceAtIndex x 0 items) (n + 1)




reduceWeight :: [Int] -> Int -> Int
reduceWeight [] _ = 0
--reduceWeight _ 0  = 0
reduceWeight (x:xs) y
                | (y - x) <= 0 = 0
                | otherwise    = 1 + reduceWeight xs (y - x)

bog1 seed = let items' = items in map (\ x -> range 1 (sum items) seed) items'

{- repeatNTimes 0 _    = "s"
repeatNTimes n seed = do
  masterindex' <- masterindex seed
  repeatNTimes (n-1) (seed + 1) -}
{- 
reduceWeight :: [Int] -> Int -> Int
reduceWeight (x:xs) y = do
    if (y - x) <= 0
        then 0
        else 1 + reduceWeight xs (y - x)

getResult range items = do
    x <- masterindex (reduceWeight items range)
    let masterindex' = T.unpack x
    return masterindex'

test4 :: Int -> [Int] -> Int -> [Int]
test4 seed items n = do
    let seed' = seed + (if n `mod` 5 == 0 then 2 else 1)
    let x = reduceWeight items $ range 1 (sum items) seed'
    if n == 17
        then []
    else
        x : test4 seed' (replaceAtIndex x 0 items) (n + 1)

new1 seed = map (\ x -> range 1 (sum items) seed)

test5 :: Int -> IO String
test5 n = do
    x <- masterindex n
    return $ T.unpack x
    --reduceWeight items $ range 1 (sum items) seed

newsaveBacktracking :: [String] -> IO [String]
newsaveBacktracking string = do
    return string

test8 :: String -> IO String
test8 string = do
    return string

test9 :: T.Text -> IO [String]
test9 index = do
    let index' = T.unpack index
    return [index']

test10 :: IO [String] -> IO [String] -> IO [String]
test10 x y = do
    x' <- x
    y' <- y
    return $ x' ++ y'

test7 :: Int -> [Int] -> IO [String]
test7 n xs = do
    x <- masterindex (head xs)
    if n == 0
        then newsaveBacktracking []
        else test10 (test9 x) (test7 (n - 1) (tail xs))


ouput' xs = do
    items' <- items 
    return $ [seed |
                    seed <- [99999..999999],
                    test4 seed items' 1 == xs
                    ]

output seed = do
    items' <- items
    return $ test4 seed items' 1
    --test7 (length x) x

    {- let totalWeight = reduceWeight items' (range 1 n seed)
    x <- masterindex totalWeight
    let masterindex' = T.unpack x 
    return masterindex' -}

weight' = do
  x <- masterindex 5
  let y = T.unpack x
  return $ head y
 -}

rangeTest x = map (range 1 825) [0..x]
nextUIntTest = map nextUInt [0..10000]
toUInt32Test = map toUInt32 [4294962295..4294972295]


main = do
    print "mine 1: "
    x1 <- readLn :: IO Int
    print "mine 2: "
    x2 <- readLn :: IO Int
    print "mine 3: "
    x3 <- readLn :: IO Int
    print "mine 4: "
    x4 <- readLn :: IO Int
    print "dungeon 1: "
    x5 <- readLn :: IO Int
    print "dungeon 2: "
    x6 <- readLn :: IO Int
    print "dungeon 3: "
    x7 <- readLn :: IO Int
    print "dungeon 4: "
    x8 <- readLn :: IO Int
    print "halls 1: "
    x9 <- readLn :: IO Int
    print "halls 2: "
    x10 <- readLn :: IO Int
    print "halls 3: "
    x11 <- readLn :: IO Int
    print "halls 4: "
    x12 <- readLn :: IO Int
    print "caverns 1: "
    x13 <- readLn :: IO Int
    print "caverns 2: "
    x14 <- readLn :: IO Int
    print "caverns 3: "
    x15 <- readLn :: IO Int
    print "caverns 4: "
    x16 <- readLn :: IO Int
    print "core 1: "
    x17 <- readLn :: IO Int
    print "core 2: "
    x18 <- readLn :: IO Int
    print "core 3: "
    x19 <- readLn :: IO Int
    print "core 4: "
    x20 <- readLn :: IO Int
    print "amount: "
    n <- readLn :: IO Int

    print "Start"
    timeIt $ print (newsaveBacktracking [x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,x15,x16,x17,x18,x19,x20] n)
    print "End"