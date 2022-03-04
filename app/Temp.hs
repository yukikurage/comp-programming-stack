-----------------
-- GHC Options --
-----------------

{-# OPTIONS_GHC -O2                       #-}
{-# OPTIONS_GHC -Wno-unused-imports       #-}
{-# OPTIONS_GHC -Wno-missing-import-lists #-}

-------------------------
-- Language Extensions --
-------------------------

{-# LANGUAGE BlockArguments       #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE MultiWayIf           #-}
{-# LANGUAGE NegativeLiterals     #-}
{-# LANGUAGE OverloadedLists      #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE StrictData           #-}
{-# LANGUAGE TupleSections        #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE ViewPatterns         #-}

module Main where

------------------
-- Import Lists --
------------------

import           Control.Arrow                  ( (>>>) )
import qualified Control.Monad                 as M
import qualified Control.Monad.Primitive       as Prim
import qualified Control.Monad.ST              as ST
import qualified Data.Array.IArray             as A
import qualified Data.Array.IO                 as AIO
import qualified Data.Array.ST                 as AST
import qualified Data.Bits                     as Bits
import qualified Data.Char                     as Char
import qualified Data.Complex                  as Comp
import qualified Data.Foldable                 as Foldable
import qualified Data.Function                 as Func
import qualified Data.Functor                  as Functor
import qualified Data.IORef                    as IORef
import qualified Data.IntPSQ                   as PSQueue
import qualified Data.Ix                       as Ix
import qualified Data.List                     as L
import qualified Data.Map.Strict               as Map
import qualified Data.Maybe                    as Maybe
import qualified Data.Primitive.MutVar         as MutVar
import qualified Data.Proxy                    as Proxy
import qualified Data.Ratio                    as Ratio
import qualified Data.STRef                    as STRef
import qualified Data.Sequence                 as Seq
import           Data.Sequence                  ( (<|)
                                                , (><)
                                                , ViewL((:<), EmptyL)
                                                , ViewR((:>), EmptyR)
                                                , viewl
                                                , viewr
                                                , (|>)
                                                )
import qualified Data.Set                      as Set
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as TIO
import qualified Data.Tree                     as Tree
import qualified Data.Vector                   as V
import qualified Data.Vector.Algorithms.Merge  as VAM
import qualified Data.Vector.Algorithms.Radix  as VAR
import qualified Data.Vector.Algorithms.Search as VAS
import           Data.Vector.Generic            ( (!)
                                                , (!?)
                                                )
import qualified Data.Vector.Generic           as VG
import qualified Data.Vector.Generic.Mutable   as VGM
import qualified Data.Vector.Mutable           as VM
import qualified Debug.Trace                   as Trace
import qualified GHC.TypeNats                  as TypeNats
import           Prelude                 hiding ( (!!)
                                                , head
                                                , print
                                                , tail
                                                , uncons
                                                )

----------
-- Main --
----------

yes, no :: T.Text
yes = "Yes"
no = "No"

main :: IO ()
main = do
  pure ()

-------------
-- Library --
-------------

type V = V.Vector
type VM = VM.MVector
type T = T.Text
type I = Int
type IG = Integer
type D = Double
type B = Bool
type S = String

default (V.Vector, [], T.Text, String, Int, Double)

infixl 1 #
(#) :: a -> (a -> b) -> b
(#) a f = f a

---------
-- I/O --
---------

-- | ex) get @I, get @(V I) ..
get :: ReadText a => IO a
get = readText <$> TIO.getLine

-- | ex) getLn @(V I) n, getLn @[I] n
getLines :: Int -> forall a . ReadTextLines a => IO a
getLines n = readTextLines <$> M.replicateM n TIO.getLine

-- | 改行なし出力
output :: ShowText a => a -> IO ()
output = TIO.putStr . showText

-- | 改行なし出力
outputLines :: ShowTextLines a => a -> IO ()
outputLines = TIO.putStr . T.unlines . showTextLines

-- | 改行あり出力
print :: ShowText a => a -> IO ()
print = TIO.putStrLn . showText

-- | 改行あり出力
printLines :: ShowTextLines a => a -> IO ()
printLines = TIO.putStrLn . T.unlines . showTextLines

---------------
-- Read/Show --
---------------

-- | Text版Read
class ReadText a where
  readText :: T.Text -> a

class ShowText a where
  showText :: a -> T.Text

instance ReadText Int where
  readText s = read $ T.unpack s

instance ReadText Integer where
  readText = fromIntegral . (readText @Int)

instance ReadText Double where
  readText = read . T.unpack

instance ReadText T.Text where
  readText = id

instance (ReadText a) => ReadText (V.Vector a) where
  readText = readVec

instance ReadText a => ReadText [a] where
  readText = map readText . T.words

instance (ReadText a, ReadText b) => ReadText (a, b) where
  readText (T.words -> [a, b]) = (readText a, readText b)
  readText _ = error "Invalid Format :: readText :: BS -> (a, b)"

instance (ReadText a, ReadText b, ReadText c) => ReadText (a, b, c) where
  readText (T.words -> [a, b, c]) = (readText a, readText b, readText c)
  readText _ = error "Invalid Format :: readText :: BS -> (a, b, c)"

instance (ReadText a, ReadText b, ReadText c, ReadText d) => ReadText (a, b, c, d) where
  readText (T.words -> [a, b, c, d]) =
    (readText a, readText b, readText c, readText d)
  readText _ = error "Invalid Format :: readText :: BS -> (a, b, c, d)"

instance ShowText Int where
  showText = T.pack . show

instance ShowText Integer where
  showText = T.pack . show

instance ShowText Double where
  showText = T.pack . show

instance ShowText Bool where
  showText True  = yes
  showText False = no

instance ShowText T.Text where
  showText = id

instance (ShowText a) => ShowText (V.Vector a) where
  showText = showVec

instance ShowText a => ShowText [a] where
  showText = T.unwords . map showText

instance (ShowText a, ShowText b) => ShowText (a, b) where
  showText (a, b) = showText a `T.append` " " `T.append` showText b

instance (ShowText a, ShowText b, ShowText c) => ShowText (a, b, c) where
  showText (a, b, c) =
    showText a
      `T.append` " "
      `T.append` showText b
      `T.append` " "
      `T.append` showText c

instance (ShowText a, ShowText b, ShowText c, ShowText d) => ShowText (a, b, c, d) where
  showText (a, b, c, d) =
    showText a
      `T.append` " "
      `T.append` showText b
      `T.append` " "
      `T.append` showText c
      `T.append` " "
      `T.append` showText d

readVec :: (VG.Vector v a, ReadText a) => T.Text -> v a
readVec = VG.fromList . readText

showVec :: (VG.Vector v a, ShowText a) => v a -> T.Text
showVec = showText . VG.toList

class ReadTextLines a where
  readTextLines :: [T.Text] -> a

class ShowTextLines a where
  showTextLines :: a -> [T.Text]

instance ReadText a => ReadTextLines [a] where
  readTextLines = map readText

instance ReadText a => ReadTextLines (V.Vector a) where
  readTextLines = readVecLines

instance ReadTextLines T.Text where
  readTextLines = T.unlines

instance ShowText a => ShowTextLines [a] where
  showTextLines = map showText

instance ShowText a => ShowTextLines (V.Vector a) where
  showTextLines = showVecLines

instance ShowTextLines T.Text where
  showTextLines s = [s]

readVecLines :: (VG.Vector v a, ReadText a) => [T.Text] -> v a
readVecLines = VG.fromList . map readText

showVecLines :: (VG.Vector v a, ShowText a) => v a -> [T.Text]
showVecLines = map showText . VG.toList

------------
-- ModInt --
------------

newtype Mod a (p :: TypeNats.Nat) = ModInt a deriving (Eq, Show)

instance ShowText a => ShowText (Mod a p) where
  showText (ModInt x) = showText x

instance (TypeNats.KnownNat p, Integral a) => Num (Mod a p) where
  (ModInt x) + (ModInt y) = ModInt $ (x + y) `mod` p
    where p = fromIntegral $ TypeNats.natVal (Proxy.Proxy :: Proxy.Proxy p)
  (ModInt x) * (ModInt y) = ModInt $ (x * y) `mod` p
    where p = fromIntegral $ TypeNats.natVal (Proxy.Proxy :: Proxy.Proxy p)
  negate (ModInt x) = ModInt $ -x `mod` p
    where p = fromIntegral $ TypeNats.natVal (Proxy.Proxy :: Proxy.Proxy p)
  abs = id
  signum _ = 1
  fromInteger n = ModInt $ fromInteger n `mod` p
    where p = fromIntegral $ TypeNats.natVal (Proxy.Proxy :: Proxy.Proxy p)

instance (TypeNats.KnownNat p, Integral a) => Fractional (Mod a p) where
  recip (ModInt n)
    | gcd n p /= 1 = error
      "recip :: Mod a p -> Mod a p : The inverse element does not exisTIO."
    | otherwise = ModInt . fst $ extendedEuc n (-p)
    where p = fromIntegral $ TypeNats.natVal (Proxy.Proxy :: Proxy.Proxy p)
  fromRational r = ModInt n / ModInt d   where
    n = fromInteger $ Ratio.numerator r
    d = fromInteger $ Ratio.denominator r

------------
-- InfInt --
------------

data Sign = Positive | Negative | NA deriving (Eq, Show)

negateSign :: Sign -> Sign
negateSign Positive = Negative
negateSign Negative = Positive
negateSign NA       = NA

addSign :: Sign -> Sign -> Sign
addSign Positive Positive = Positive
addSign Positive Negative = NA
addSign Negative Positive = NA
addSign Negative Negative = Negative
addSign _        _        = NA

mulSign :: Sign -> Sign -> Sign
mulSign Positive Positive = Positive
mulSign Positive Negative = Negative
mulSign Negative Positive = Negative
mulSign Negative Negative = Positive
mulSign _        _        = NA

compareSign :: Sign -> Sign -> Ordering
compareSign Positive Positive = EQ
compareSign Positive Negative = GT
compareSign Negative Positive = LT
compareSign Negative Negative = EQ
compareSign _        _        = EQ

data Inf a = Infinity Sign | Finity a deriving (Eq, Show)

instance ReadText a => ReadText (Inf a) where
  readText = \case
    "Infinity"  -> Infinity Positive
    "-Infinity" -> Infinity Negative
    "NA"        -> Infinity NA
    x           -> Finity $ readText x

instance ShowText a => ShowText (Inf a) where
  showText = \case
    Infinity Positive -> "Infinity"
    Infinity Negative -> "-Infinity"
    Infinity NA       -> "NA"
    Finity   x        -> showText x

instance (Num a, Ord a) => Num (Inf a) where
  x + y = case x of
    Infinity sign -> case y of
      Infinity sign' -> Infinity $ addSign sign sign'
      Finity   y'    -> Infinity sign
    Finity x' -> case y of
      Infinity sign' -> Infinity sign'
      Finity   y'    -> Finity $ x' + y'

  x * y = case x of
    Infinity sign -> case y of
      Infinity sign' -> Infinity $ mulSign sign sign'
      Finity y' | y' < 0    -> Infinity $ negateSign sign
                | y' == 0   -> Infinity NA
                | otherwise -> Infinity sign
    Finity x' -> case y of
      Infinity sign | x' < 0    -> Infinity $ negateSign sign
                    | x' == 0   -> Infinity NA
                    | otherwise -> Infinity sign
      Finity y' -> Finity $ x' * y'
  negate = \case
    Infinity sign -> Infinity $ negateSign sign
    Finity   x    -> Finity $ negate x
  abs = \case
    Infinity sign -> Infinity Positive
    Finity   x    -> Finity $ abs x
  signum = \case
    Infinity Positive -> Finity 1
    Infinity Negative -> Finity -1
    Infinity NA       -> Infinity NA
    Finity   x        -> Finity $ signum x
  fromInteger n = Finity $ fromInteger n

instance (Num a, Ord a) => Ord (Inf a) where
  compare = \case
    Infinity sign -> \case
      Infinity sign' -> compareSign sign sign'
      Finity   _     -> if sign == Positive then GT else LT
    Finity x -> \case
      Infinity sign -> if sign == Positive then LT else GT
      Finity   y    -> compare x y

(.!) :: (VG.Vector v p, Num p) => v p -> Int -> p
xs .! i | i >= 0 && i < VG.length xs = xs ! i
        | otherwise                  = 0

infinity :: Num a => Inf a
infinity = Infinity Positive

na :: Inf a
na = Infinity NA

dropWhileRev :: VG.Vector v a => (a -> Bool) -> v a -> v a
dropWhileRev f xs | VG.null xs = VG.empty
                  | f x        = dropWhileRev f xs'
                  | otherwise  = xs
 where
  x   = VG.last xs
  xs' = VG.init xs

------------------
-- Disjoint Set --
------------------

type DisjointSet = V.Vector Int
data DisjointSetM m = DSet
  { dsParents :: VM.MVector m Int
  , dsDepths  :: VM.MVector m Int
  }

dsFromEdges :: Int -> V.Vector (Int, Int) -> DisjointSet
dsFromEdges n edges = V.create do
  ds <- newDSet n
  V.forM_ edges $ uncurry (union ds)
  return $ dsParents ds

newDSet :: Prim.PrimMonad m => Int -> m (DisjointSetM (Prim.PrimState m))
newDSet n = DSet <$> V.thaw (V.generate n id) <*> VM.replicate n 1

root :: DisjointSet -> Int -> Int
root xs i | xs ! i == i = i
          | otherwise   = root xs $ xs ! i

find :: DisjointSet -> Int -> Int -> Bool
find xs i j = root xs i == root xs j

-- | ルートを調べる時につなぎ直す
rootM :: Prim.PrimMonad m => DisjointSetM (Prim.PrimState m) -> Int -> m Int
rootM ds i = VM.read (dsParents ds) i >>= \p -> if p == i
  then return i
  else rootM ds p >>= \r -> VM.write (dsParents ds) i r >> return r

findM
  :: Prim.PrimMonad m => DisjointSetM (Prim.PrimState m) -> Int -> Int -> m Bool
findM ds i j = (==) <$> rootM ds i <*> rootM ds j

union
  :: Prim.PrimMonad m => DisjointSetM (Prim.PrimState m) -> Int -> Int -> m ()
union ds i j = do
  rooti <- rootM ds i
  rootj <- rootM ds j
  depi  <- VM.read (dsDepths ds) rooti
  depj  <- VM.read (dsDepths ds) rootj
  if
    | depi == depj
    -> VM.modify (dsDepths ds) (+ 1) rooti
      >> VM.write (dsParents ds) rootj rooti
    | depi > depj
    -> VM.write (dsParents ds) rootj rooti
    | otherwise
    -> VM.write (dsParents ds) rooti rootj

----------------------------
-- Monadic Priority Queue --
----------------------------

type MPSQueue m p v = MutVar.MutVar m (PSQueue.IntPSQ p v)

mpsqNull :: Prim.PrimMonad m => MPSQueue (Prim.PrimState m) p v -> m Bool
mpsqNull q = PSQueue.null <$> MutVar.readMutVar q

mpsqEmpty :: Prim.PrimMonad m => m (MPSQueue (Prim.PrimState m) p v)
mpsqEmpty = MutVar.newMutVar PSQueue.empty

mpsqSingleton
  :: Prim.PrimMonad m
  => Ord p => Int -> p -> v -> m (MPSQueue (Prim.PrimState m) p v)
mpsqSingleton k p v = MutVar.newMutVar $ PSQueue.singleton k p v

mpsqInsert
  :: Prim.PrimMonad m
  => Ord p => MPSQueue (Prim.PrimState m) p v -> Int -> p -> v -> m ()
mpsqInsert m k p v = MutVar.modifyMutVar' m (PSQueue.insert k p v)

-- | 要素は削除せず，優先度が一番小さいものを取り出す
mpsqFindMin
  :: Prim.PrimMonad m
  => Ord p => MPSQueue (Prim.PrimState m) p v -> m (Maybe (Int, p, v))
mpsqFindMin q = PSQueue.findMin <$> MutVar.readMutVar q

-- | 要素を削除して，優先度が一番小さいものを取り出す
mpsqMinView
  :: Prim.PrimMonad m
  => Ord p => MPSQueue (Prim.PrimState m) p v -> m (Maybe (Int, p, v))
mpsqMinView q = do
  res <- PSQueue.minView <$> MutVar.readMutVar q
  case res of
    Nothing            -> return Nothing
    Just (k, p, v, q') -> do
      MutVar.writeMutVar q q'
      return $ Just (k, p, v)


----------------------
-- Monadic Sequence --
----------------------

type MSequence m a = MutVar.MutVar m (Seq.Seq a)

msqEmpty :: Prim.PrimMonad m => m (MSequence (Prim.PrimState m) a)
msqEmpty = MutVar.newMutVar Seq.empty

msqSingleton :: Prim.PrimMonad m => a -> m (MSequence (Prim.PrimState m) a)
msqSingleton x = MutVar.newMutVar $ Seq.singleton x

infixr 5 <<|
infixl 5 |>>

-- | Cons
(<<|) :: Prim.PrimMonad m => a -> MSequence (Prim.PrimState m) a -> m ()
x <<| xs = MutVar.modifyMutVar' xs (x Seq.<|)

-- | Snoc
(|>>) :: Prim.PrimMonad m => MSequence (Prim.PrimState m) a -> a -> m ()
xs |>> x = MutVar.modifyMutVar' xs (Seq.|> x)

msqViewL :: Prim.PrimMonad m => MSequence (Prim.PrimState m) a -> m (Maybe a)
msqViewL xs = do
  xs' <- MutVar.readMutVar xs
  case viewl xs' of
    EmptyL -> pure Nothing
    x :< _ -> pure (Just x)

msqViewR :: Prim.PrimMonad m => MSequence (Prim.PrimState m) a -> m (Maybe a)
msqViewR xs = do
  xs' <- MutVar.readMutVar xs
  case viewr xs' of
    EmptyR    -> pure Nothing
    xs'' :> x -> pure (Just x)

msqPopL :: Prim.PrimMonad m => MSequence (Prim.PrimState m) a -> m (Maybe a)
msqPopL xs = do
  xs' <- MutVar.readMutVar xs
  case viewl xs' of
    EmptyL    -> pure Nothing
    x :< xs'' -> do
      MutVar.writeMutVar xs xs''
      pure (Just x)

msqPopR :: Prim.PrimMonad m => MSequence (Prim.PrimState m) a -> m (Maybe a)
msqPopR xs = do
  xs' <- MutVar.readMutVar xs
  case viewr xs' of
    EmptyR    -> pure Nothing
    xs'' :> x -> do
      MutVar.writeMutVar xs xs''
      pure (Just x)

msqNull :: Prim.PrimMonad m => MSequence (Prim.PrimState m) a -> m Bool
msqNull xs = do
  xs' <- MutVar.readMutVar xs
  pure $ Seq.null xs'

-----------
-- Graph --
-----------

type Graph a = V.Vector [(Int, a)] --aは辺の情報
type UGraph = Graph ()

gEmpty :: Graph a
gEmpty = V.singleton []

gFromEdges :: Int -> V.Vector (Int, Int, a) -> Graph a
gFromEdges n edges = ST.runST do
  v <- VM.replicate n []
  V.forM_ edges \(i, j, a) -> VM.modify v ((j, a) :) i
  V.freeze v

-- | 辺をすべて反転させる
gReverse :: Graph a -> Graph a
gReverse g = ST.runST do
  let n = V.length g
  v <- VM.replicate n []
  V.forM_ [0 .. n - 1] \i -> M.forM_ (g ! i) \(j, a) -> VM.modify v ((i, a) :) j
  V.freeze v

dijkstra :: Graph Int -> Int -> V.Vector (Inf Int)
dijkstra g i = V.create do
  let n = V.length g

  -- 辺の距離の初期化
  dists <- VM.replicate n infinity
  VM.write dists i 0

  -- キューの初期化
  queue <- mpsqSingleton i 0 ()

  let
    -- 頂点情報のアップデート処理
      update v alt = do
        VM.write dists v alt
        mpsqInsert queue v alt ()

      -- 確定した頂点を取り出したときの処理
      processing u = do
        dist_u <- VM.read dists u
        M.forM_
          (g ! u)
          (\(v, cost) -> do
            dist_v <- VM.read dists v
            let alt = dist_u + fromIntegral cost
            M.when (dist_v > alt) $ update v alt
          )

  while do
    res <- mpsqMinView queue
    case res of
      Nothing        -> return False
      Just (u, _, _) -> do
        processing u
        return True
  return dists

dfs :: Graph a -> Int -> Tree.Tree Int
dfs g i = ST.runST do
  reached <- VM.replicate (V.length g) False
  let loop now = do
        VM.write reached now True
        next     <- M.filterM (fmap not . VM.read reached) . map fst $ g ! now
        children <- M.mapM loop next
        return $ Tree.Node now children
  loop i

----------
-- Maze --
----------

type Maze = V.Vector (V.Vector Char)
-- ^ 競プロでよく出るCharの迷路

mzHeight :: Maze -> Int
mzHeight = V.length

mzWidth :: Maze -> Int
mzWidth v = maybe 0 V.length (v !? 0)

type Rules a = Char -> Char -> Maybe a

readMaze :: T.Text -> Maze
readMaze str = V.fromList $ map (V.fromList . T.unpack) $ T.lines str

getMaze :: Int -> IO Maze
getMaze h = readMaze <$> getLines h

mazePos :: Maze -> (Int, Int) -> Int
mazePos mz (i, j) = i * mzWidth mz + j

mazeToGraph :: Rules a -> Maze -> Graph a
mazeToGraph rules maze =
  let h = mzHeight maze
      w = mzWidth maze
      l = head [1]
  in  gFromEdges (h * w)
        $ V.fromList
        $ Maybe.catMaybes
        $ [ edge
          | i        <- [0 .. h - 1]
          , j        <- [0 .. w - 1]
          , (i', j') <- [(i - 1, j), (i, j - 1), (i + 1, j), (i, j + 1)]
          , i' >= 0
          , i' < h
          , j' >= 0
          , j' < w
          , let c    = maze ! i ! j
          , let c' = maze ! i' ! j'
          , let edge = (w * i + j, w * i' + j', ) <$> rules c c'
          ]


----------
-- Tree --
----------

depth :: Eq a => Tree.Tree a -> a -> Maybe Int
depth (Tree.Node a []) b | a == b = Just 0
depth (Tree.Node _ []) _          = Nothing
depth (Tree.Node a xs) b          = case Maybe.mapMaybe (`depth` b) xs of
  [] -> Nothing
  xs -> Just $ 1 + minimum xs


--------------
-- MultiSet --
--------------

type MultiSet a = Map.Map a Int

msEmpty :: MultiSet a
msEmpty = Map.empty

msSingleton :: a -> MultiSet a
msSingleton a = Map.singleton a 1

msFromList :: Ord a => [a] -> MultiSet a
msFromList = Map.fromListWith (+) . map (, 1)

msInsert :: Ord a => a -> MultiSet a -> MultiSet a
msInsert a = Map.insertWith (+) a 1

msDelete :: Ord a => a -> MultiSet a -> MultiSet a
msDelete = Map.update (\n -> if n > 1 then Just (n - 1) else Nothing)

msMember :: Ord a => a -> MultiSet a -> Bool
msMember = Map.member

msLookupLT :: Ord a => a -> MultiSet a -> Maybe a
msLookupLT a = fmap fst . Map.lookupLT a

msLookupGT :: Ord a => a -> MultiSet a -> Maybe a
msLookupGT a = fmap fst . Map.lookupGT a

msLookupLE :: Ord a => a -> MultiSet a -> Maybe a
msLookupLE a = fmap fst . Map.lookupLE a

msLookupGE :: Ord a => a -> MultiSet a -> Maybe a
msLookupGE a = fmap fst . Map.lookupGE a

msNull :: MultiSet a -> Bool
msNull = Map.null

msUnion :: Ord a => MultiSet a -> MultiSet a -> MultiSet a
msUnion = Map.unionWith (+)

---------------------
-- MutableMultiSet --
---------------------

type MutableMultiSet m a = MutVar.MutVar m (MultiSet a)

mmsEmpty :: Prim.PrimMonad m => m (MutableMultiSet (Prim.PrimState m) a)
mmsEmpty = MutVar.newMutVar msEmpty

mmsSingleton
  :: Prim.PrimMonad m => a -> m (MutableMultiSet (Prim.PrimState m) a)
mmsSingleton a = MutVar.newMutVar $ msSingleton a

mmsFromList
  :: Prim.PrimMonad m
  => Ord a => [a] -> m (MutableMultiSet (Prim.PrimState m) a)
mmsFromList = MutVar.newMutVar . msFromList

mmsInsert
  :: Prim.PrimMonad m
  => Ord a => MutableMultiSet (Prim.PrimState m) a -> a -> m ()
mmsInsert mms a = MutVar.modifyMutVar' mms $ msInsert a

mmsDelete
  :: Prim.PrimMonad m
  => Ord a => MutableMultiSet (Prim.PrimState m) a -> a -> m ()
mmsDelete mms a = MutVar.modifyMutVar' mms $ msDelete a

mmsMember
  :: Prim.PrimMonad m
  => Ord a => MutableMultiSet (Prim.PrimState m) a -> a -> m Bool
mmsMember mms a = MutVar.readMutVar mms Functor.<&> msMember a

mmsLookupLT
  :: Prim.PrimMonad m
  => Ord a => MutableMultiSet (Prim.PrimState m) a -> a -> m (Maybe a)
mmsLookupLT mms a = MutVar.readMutVar mms Functor.<&> msLookupLT a

mmsLookupGT
  :: Prim.PrimMonad m
  => Ord a => MutableMultiSet (Prim.PrimState m) a -> a -> m (Maybe a)
mmsLookupGT mms a = MutVar.readMutVar mms Functor.<&> msLookupGT a

mmsLookupLE :: Prim.PrimMonad m => Ord a => MutableMultiSet (Prim.PrimState m) a -> a -> m (Maybe a)
mmsLookupLE mms a = MutVar.readMutVar mms Functor.<&> msLookupLE a

mmsLookupGE :: Prim.PrimMonad m => Ord a => MutableMultiSet (Prim.PrimState m) a -> a -> m (Maybe a)
mmsLookupGE mms a = MutVar.readMutVar mms Functor.<&> msLookupGE a

mmsNull :: Prim.PrimMonad m => MutableMultiSet (Prim.PrimState m) a -> m Bool
mmsNull mms = MutVar.readMutVar mms Functor.<&> msNull

mmsUnion :: Prim.PrimMonad m => Ord a => MutableMultiSet (Prim.PrimState m) a -> MutableMultiSet (Prim.PrimState m) a -> m ()
mmsUnion mms1 mms2 = do
  ms1 <- MutVar.readMutVar mms1
  ms2 <- MutVar.readMutVar mms2
  MutVar.modifyMutVar' mms1 $ msUnion ms2

------------
-- Others --
------------

head :: V.Vector a -> Maybe a
head v = if V.null v then Nothing else Just (v V.! 0)

tail :: V.Vector a -> Maybe (V.Vector a)
tail v = if V.null v then Nothing else Just (V.tail v)

uncons :: V.Vector a -> Maybe (a, V.Vector a)
uncons v = if V.null v then Nothing else Just (v V.! 0, V.tail v)

while :: Monad m => m Bool -> m ()
while f = f >>= \frag -> M.when frag $ while f

divisor :: Integral a => a -> [a]
divisor n
  | n <= 0 = error
    "divisor : Definition range does not include negative numbers or zero"
  | otherwise = half ++ rev where
  mid  = floor . sqrt @Double . fromIntegral $ n
  half = filter ((== 0) . mod n) [1 .. mid]
  rev =
    reverse
      . map (n `div`)
      . (if mid ^ (2 :: Int) == n then init else id)
      $ half

primeFact :: forall a b . (Integral a, Integral b) => a -> [(a, b)]
primeFact 1 = []
primeFact n
  | n == 1 = []
  | n <= 0 = error
    "primefact : Definition range does not include negative numbers or zero"
  | otherwise = case L.find ((== 0) . mod n) ([2 .. mid] :: [a]) of
    Nothing -> [(n, 1)]
    Just p  -> (p, m) : primeFact next     where
      m    = loop n p
      next = n `div` (p ^ m)
 where
  loop m q | m `mod` q == 0 = 1 + loop (m `div` q) q
           | otherwise      = 0
  mid = floor . sqrt @Double . fromIntegral $ n

-- | 素数を取得
primes :: forall a . Integral a => a -> [a]
primes n | n <= 1    = []
         | otherwise = filter ((frags !) . fromIntegral) [2 .. n] where
  frags = V.create do
    v <- VM.replicate (fromIntegral (n + 1)) True
    VM.write v 0 False
    VM.write v 1 False
    V.forM_
      [2 .. floor . sqrt @Double . fromIntegral $ n]
      \i -> do
        frag <- VM.read v i
        M.when frag
          $ V.forM_ [2 * i, 3 * i .. fromIntegral n] \j -> VM.write v j False
    return v

-- | 拡張されたユークリッドの互除法
-- | ax + by = gcd a b を解く
extendedEuc :: (Integral b) => b -> b -> (b, b)
extendedEuc a b | a >= 0 && b == 0 = (1, 0)
                | a < 0 && b == 0  = (-1, 0)
                | otherwise        = (t, s - q * t) where
  (q, r) = divMod a b
  (s, t) = extendedEuc b r
