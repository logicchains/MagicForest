{-# LANGUAGE BangPatterns #-}
import qualified Data.Vector as V
import qualified Data.Set as S
import qualified Data.Vector.Algorithms.Intro as I
import System.Environment
import Debug.Trace

data Forest = Forest{ 
     goats :: !Int
    , wolves :: !Int
    , lions    :: !Int
    } deriving (Show, Ord, Eq)

addFors :: Forest -> Forest -> Forest
addFors Forest {goats = g1, wolves = w1, lions = l1} 
	Forest {goats = g2, wolves = w2, lions = l2} =
	Forest {goats = g1+g2, wolves =w1+w2, lions = l1+l2}

forStable :: Forest -> Bool
forStable Forest {goats = g, wolves = w, lions = l} =
	  if g == 0 then w == 0 || l == 0
	  else w == 0 && l == 0

forInvalid :: Forest -> Bool
forInvalid Forest {goats = g, wolves = w, lions = l} =
	   g < 0 || w < 0 || l < 0

meal :: Forest -> V.Vector Forest
meal Forest {goats = g, wolves = w, lions = l} =
     V.snoc (V.snoc (V.singleton Forest {goats = g - 1, wolves =  w - 1, lions = l + 1})
     Forest {goats = g-1, wolves = w+1,lions= l-1})
     Forest {goats = g+1,wolves = w-1,lions= l-1}

sortFors :: V.Vector Forest -> V.Vector Forest
sortFors fors = V.modify I.sort fors

uniqForsRec :: (V.Vector Forest, V.Vector Forest) -> (V.Vector Forest, V.Vector Forest)
uniqForsRec (old, new)
   | old == V.empty = (old, new)
   | new == V.empty = moveIt
   | V.last old == V.last new = uniqForsRec (V.init old,  new)
-- | V.last old == V.last new = if (trace ("Duplicates encountered: " ++ show  (V.last old)) True) then uniqForsRec (V.init old, new) else (old,new) 
-- | otherwise = if (trace ("doing it: " ++ show old) True) then moveIt else (old,new)
   | otherwise =  moveIt 
   where moveIt = uniqForsRec (V.init old, V.snoc new (V.last old) )

uniqFors :: V.Vector Forest -> V.Vector Forest
uniqFors for = snd $ uniqForsRec (for, V.empty)

meals :: V.Vector Forest -> V.Vector Forest
meals fors = V.fromList $  S.toList $  S.fromList $ V.toList $ snd $ V.unstablePartition forInvalid $ V.concatMap meal fors
--meals fors = uniqFors $ sortFors $ snd $ V.unstablePartition forInvalid $ V.concatMap meal fors

devouringImpossible :: V.Vector Forest -> Bool
devouringImpossible for = not $ not (V.null for) && (not $ V.any forStable for)

stableForests :: V.Vector Forest -> V.Vector Forest
stableForests for = V.filter forStable for

findStableForests :: Forest -> V.Vector Forest
findStableForests for = stableForests $ until (devouringImpossible) meals $ V.singleton for

main :: IO ()
main = do
    (g:w:l:_) <- getArgs
    let stableForests = findStableForests Forest {goats = read g, wolves = read w, lions = read l}
    putStr $ show stableForests