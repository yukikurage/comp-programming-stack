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
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ViewPatterns         #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

------------------
-- Import Lists --
------------------

import           Control.Arrow                  ( (>>>) )
import qualified Control.Monad                 as M
import qualified Control.Monad.Primitive       as Prim
import qualified Control.Monad.RWS             as RWS
import qualified Control.Monad.Reader          as Reader
import qualified Control.Monad.ST              as ST
import qualified Control.Monad.State           as State
import qualified Control.Monad.Writer          as Writer
import qualified Data.Array.IArray             as A
import qualified Data.Array.IO                 as AIO
import qualified Data.Array.ST                 as AST
import qualified Data.Bits                     as Bits
import qualified Data.ByteString.Char8         as BS
import qualified Data.Char                     as Char
import qualified Data.Complex                  as Comp
import           Data.Foldable
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
import qualified Data.Vector.Unboxed
import qualified Data.Vector.Unboxing          as VU
import qualified Data.Vector.Unboxing.Mutable  as VUM
import qualified Debug.Trace                   as Trace
import           Debug.Trace                    ( traceShow )
import qualified GHC.Generics
import qualified GHC.TypeNats                  as TypeNats
import           Prelude                 hiding ( (!)
                                                , head
                                                , map
                                                , print
                                                , tail
                                                , uncons
                                                )
import qualified Prelude

----------
-- Main --
----------

yes, no :: BS.ByteString
yes = "Yes"
no = "No"

main :: IO ()
main = do
  pure ()

-------------
-- Library --
-------------

type V = V.Vector
type VU = VU.Vector
type VM = VM.MVector
type VUM = VUM.MVector
type T = BS.ByteString
type I = Int
type IG = Int
type D = Double
type B = Bool
type S = String

default (V.Vector, [], BS.ByteString, String, Int, Double)

infixl 1 #
(#) :: a -> (a -> b) -> b
(#) a f = f a

map :: Monad m => (a -> b) -> m a -> m b
map = fmap

---------
-- I/O --
---------

-- | ex) get @I, get @(V I) ..
get :: ReadText a => IO a
get = readText <$> BS.getLine

-- | ex) getLn @(V I) n, getLn @[I] n
getLines :: Int -> forall a . ReadTextLines a => IO a
getLines n = readTextLines <$> M.replicateM n BS.getLine

-- | 改行なし出力
output :: ShowText a => a -> IO ()
output = BS.putStr . showText

-- | 改行なし出力
outputLines :: ShowTextLines a => a -> IO ()
outputLines = BS.putStr . BS.unlines . showTextLines

-- | 改行あり出力
print :: ShowText a => a -> IO ()
print = BS.putStrLn . showText

-- | 改行あり出力
printLines :: ShowTextLines a => a -> IO ()
printLines = BS.putStrLn . BS.unlines . showTextLines

---------------
-- Read/Show --
---------------

-- | Text版Read
class ReadText a where
  readText :: BS.ByteString -> a

class ShowText a where
  showText :: a -> BS.ByteString

instance ReadText Int where
  readText s = read $ BS.unpack s

instance ReadText Integer where
  readText = fromIntegral . (readText @Int)

instance ReadText Double where
  readText = read . BS.unpack

instance ReadText BS.ByteString where
  readText = id

instance ReadText a => ReadText (V.Vector a) where
  readText = readVec

instance (ReadText a, VUM.Unboxable a) => ReadText (VU.Vector a) where
  readText = readVec

instance ReadText a => ReadText [a] where
  readText = map readText . BS.words

instance (ReadText a, ReadText b) => ReadText (a, b) where
  readText (BS.words -> [a, b]) = (readText a, readText b)
  readText _ = error "Invalid Format :: readText :: BS -> (a, b)"

instance (ReadText a, ReadText b, ReadText c) => ReadText (a, b, c) where
  readText (BS.words -> [a, b, c]) = (readText a, readText b, readText c)
  readText _ = error "Invalid Format :: readText :: BS -> (a, b, c)"

instance (ReadText a, ReadText b, ReadText c, ReadText d) => ReadText (a, b, c, d) where
  readText (BS.words -> [a, b, c, d]) =
    (readText a, readText b, readText c, readText d)
  readText _ = error "Invalid Format :: readText :: BS -> (a, b, c, d)"

instance ShowText Integer where
  showText = BS.pack . show

instance ShowText Int where
  showText = BS.pack . show

instance ShowText Double where
  showText = BS.pack . show

instance ShowText Bool where
  showText True  = yes
  showText False = no

instance ShowText BS.ByteString where
  showText = id

instance (ShowText a) => ShowText (V.Vector a) where
  showText = showVec

instance (ShowText a, VU.Unboxable a) => ShowText (VU.Vector a) where
  showText = showVec

instance ShowText a => ShowText [a] where
  showText = BS.unwords . map showText

instance (ShowText a, ShowText b) => ShowText (a, b) where
  showText (a, b) = showText a `BS.append` " " `BS.append` showText b

instance (ShowText a, ShowText b, ShowText c) => ShowText (a, b, c) where
  showText (a, b, c) =
    showText a
      `BS.append` " "
      `BS.append` showText b
      `BS.append` " "
      `BS.append` showText c

instance (ShowText a, ShowText b, ShowText c, ShowText d) => ShowText (a, b, c, d) where
  showText (a, b, c, d) =
    showText a
      `BS.append` " "
      `BS.append` showText b
      `BS.append` " "
      `BS.append` showText c
      `BS.append` " "
      `BS.append` showText d

readVec :: (VG.Vector v a, ReadText a) => BS.ByteString -> v a
readVec = VG.fromList . readText

showVec :: (VG.Vector v a, ShowText a) => v a -> BS.ByteString
showVec = showText . VG.toList

class ReadTextLines a where
  readTextLines :: [BS.ByteString] -> a

class ShowTextLines a where
  showTextLines :: a -> [BS.ByteString]

instance ReadText a => ReadTextLines [a] where
  readTextLines = map readText

instance ReadText a => ReadTextLines (V.Vector a) where
  readTextLines = readVecLines

instance (ReadText a, VU.Unboxable a) => ReadTextLines (VU.Vector a) where
  readTextLines = readVecLines

instance ReadTextLines BS.ByteString where
  readTextLines = BS.unlines

instance ShowText a => ShowTextLines [a] where
  showTextLines = map showText

instance ShowText a => ShowTextLines (V.Vector a) where
  showTextLines = showVecLines

instance (ShowText a, VU.Unboxable a) => ShowTextLines (VU.Vector a) where
  showTextLines = showVecLines

instance ShowTextLines BS.ByteString where
  showTextLines s = [s]

readVecLines :: (VG.Vector v a, ReadText a) => [BS.ByteString] -> v a
readVecLines = VG.fromList . map readText

showVecLines :: (VG.Vector v a, ShowText a) => v a -> [BS.ByteString]
showVecLines = map showText . VG.toList

------------
-- ModInt --
------------

newtype Mod a (p :: TypeNats.Nat) = ModInt a deriving (Eq, Show, VU.Unboxable)

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
      "recip :: Mod a p -> Mod a p : The inverse element does not exisBS."
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

type DisjointSet = VU.Vector Int
data DisjointSetM m = DSet
  { dsParents :: VUM.MVector m Int
  , dsDepths  :: VUM.MVector m Int
  }

newDSet :: Prim.PrimMonad m => Int -> m (DisjointSetM (Prim.PrimState m))
newDSet n = DSet <$> VU.thaw (VU.generate n id) <*> VUM.replicate n 1

root :: DisjointSet -> Int -> Int
root xs i | xs ! i == i = i
          | otherwise   = root xs $ xs ! i

find :: DisjointSet -> Int -> Int -> Bool
find xs i j = root xs i == root xs j

-- | ルートを調べる時につなぎ直す
rootM :: Prim.PrimMonad m => DisjointSetM (Prim.PrimState m) -> Int -> m Int
rootM ds i = VUM.read (dsParents ds) i >>= \p -> if p == i
  then pure i
  else rootM ds p >>= \r -> VUM.write (dsParents ds) i r >> pure r

findM
  :: Prim.PrimMonad m => DisjointSetM (Prim.PrimState m) -> Int -> Int -> m Bool
findM ds i j = (==) <$> rootM ds i <*> rootM ds j

union
  :: Prim.PrimMonad m => DisjointSetM (Prim.PrimState m) -> Int -> Int -> m ()
union ds i j = do
  rooti <- rootM ds i
  rootj <- rootM ds j
  depi  <- VUM.read (dsDepths ds) rooti
  depj  <- VUM.read (dsDepths ds) rootj
  if
    | depi == depj
    -> VUM.modify (dsDepths ds) (+ 1) rooti
      >> VUM.write (dsParents ds) rootj rooti
    | depi > depj
    -> VUM.write (dsParents ds) rootj rooti
    | otherwise
    -> VUM.write (dsParents ds) rooti rootj

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
    Nothing            -> pure Nothing
    Just (k, p, v, q') -> do
      MutVar.writeMutVar q q'
      pure $ Just (k, p, v)


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

gFromEdges :: VG.Vector v (Int, Int, a) => Int -> v (Int, Int, a) -> Graph a
gFromEdges n edges = ST.runST do
  v <- VM.replicate n []
  VG.forM_ edges \(i, j, a) -> VM.modify v ((j, a) :) i
  V.freeze v

-- | 辺をすべて反転させる
gReverse :: Graph a -> Graph a
gReverse g = ST.runST do
  let n = V.length g
  v <- VM.replicate n []
  V.forM_ [0 .. n - 1] \i -> M.forM_ (g ! i) \(j, a) -> VM.modify v ((i, a) :) j
  V.freeze v

-- | ダイクストラ法
-- | グラフと頂点をとり，それぞれの辺への最短経路を求める
-- | 辺は頂点の順序が逆になることに注意(Listのconsで要素を前にくっつけていくので)
dijkstra :: Graph Int -> Int -> V.Vector (Inf Int, [Int])
dijkstra g i = V.create do
  let n = V.length g

  -- 辺の距離の初期化
  -- dists ! j == (iからの距離, 経路)
  dists <- VM.replicate n (infinity, [])
  VM.write dists i (0, [i])

  -- キューの初期化
  -- キューの中身は，key: 頂点, 優先度: 距離, value: ()
  queue <- mpsqSingleton i 0 ()

  let -- 頂点情報のアップデート処理
      -- prev: 移動前の頂点, v: 更新対象, alt: その頂点までの(現段階での)距離
      -- prev はすでに最短距離が確定している．
      update prev v alt = do
        (_, xs) <- VM.read dists prev
        VM.write dists v (alt, v : xs)
        -- アップデートした後には優先度付きキューに追加する，
        -- 頂点をキーとしているので，同じ頂点が既にあった場合は上書きされる(典型的な C++ 実装での continue に相当)
        mpsqInsert queue v alt ()

      -- 確定した頂点を取り出したときの処理
      -- u: 最短距離が確定した頂点
      processing u = do
        -- その頂点から出る辺をすべて取り出す
        (dist_u, _) <- VM.read dists u
        M.forM_
          (g ! u)
          (\(v, cost) -> do
            (dist_v, _) <- VM.read dists v
            let alt = dist_u + fromIntegral cost
            M.when (dist_v > alt) $ update u v alt
          )

  -- 繰り返し処理
  while do
    res <- mpsqMinView queue
    case res of
      Nothing        -> pure False
      Just (u, _, _) -> do
        processing u
        pure True
  pure dists

dfs :: Graph a -> Int -> Tree.Tree Int
dfs g i = ST.runST do
  reached <- VM.replicate (V.length g) False
  let loop now = do
        VM.write reached now True
        next     <- M.filterM (fmap not . VM.read reached) . map fst $ g ! now
        children <- M.mapM loop next
        pure $ Tree.Node now children
  loop i

----------
-- Maze --
----------

type Maze = V.Vector (VU.Vector Char)
-- ^ 競プロでよく出るCharの迷路

mzHeight :: Maze -> Int
mzHeight = V.length

mzWidth :: Maze -> Int
mzWidth v = maybe 0 VU.length (v !? 0)

type Rules a = Char -> Char -> Maybe a

readMaze :: BS.ByteString -> Maze
readMaze str = V.fromList $ map (VU.fromList . BS.unpack) $ BS.lines str

getMaze :: Int -> IO Maze
getMaze h = readMaze <$> getLines h

mazePos :: Maze -> (Int, Int) -> Int
mazePos mz (i, j) = i * mzWidth mz + j

mazeToGraph :: Rules a -> Maze -> Graph a
mazeToGraph rules maze =
  let h = mzHeight maze
      w = mzWidth maze
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
msFromList xs = Map.fromListWith (+) $ map (, 1) xs

msInsert :: Ord a => a -> MultiSet a -> MultiSet a
msInsert a = Map.insertWith (+) a 1

msInserts :: Ord a => a -> Int -> MultiSet a -> MultiSet a
msInserts = Map.insertWith (+)

msDelete :: Ord a => a -> MultiSet a -> MultiSet a
msDelete a = Map.update (\n -> if n > 1 then Just (n - 1) else Nothing) a

msDeletes :: Ord a => a -> Int -> MultiSet a -> MultiSet a
msDeletes a n = Map.update (\n' -> if n' > n then Just (n' - n) else Nothing) a

msDeleteAll :: Ord a => a -> MultiSet a -> MultiSet a
msDeleteAll = Map.delete

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

-- | split MultiSet into two MultiSet
-- | (LT, GE)
msSplitLTGE :: Ord a => a -> MultiSet a -> (MultiSet a, MultiSet a)
msSplitLTGE a ms =
  let (lt, x, gt) = Map.splitLookup a ms
      ge          = case x of
        Nothing -> gt
        Just v  -> msInserts a v gt
  in  (lt, ge)

msSplitLEGT :: Ord a => a -> MultiSet a -> (MultiSet a, MultiSet a)
msSplitLEGT a ms =
  let (lt, x, gt) = Map.splitLookup a ms
      le          = case x of
        Nothing -> lt
        Just v  -> msInserts a v lt
  in  (le, gt)

msElemAtL :: Ord a => Int -> MultiSet a -> Maybe a
msElemAtL i ms = case Map.minViewWithKey ms of
  Nothing -> Nothing
  Just ((k, v), rest) | i < v     -> Just k
                      | otherwise -> msElemAtL (i - v) rest

msElemAtG :: Ord a => Int -> MultiSet a -> Maybe a
msElemAtG i ms = case Map.maxViewWithKey ms of
  Nothing -> Nothing
  Just ((k, v), rest) | i < v     -> Just k
                      | otherwise -> msElemAtG (i - v) rest

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

mmsInserts
  :: Prim.PrimMonad m
  => Ord a => MutableMultiSet (Prim.PrimState m) a -> a -> Int -> m ()
mmsInserts mms a n = MutVar.modifyMutVar' mms $ msInserts a n

mmsDelete
  :: Prim.PrimMonad m
  => Ord a => MutableMultiSet (Prim.PrimState m) a -> a -> m ()
mmsDelete mms a = MutVar.modifyMutVar' mms $ msDelete a

mmsDeletes
  :: Prim.PrimMonad m
  => Ord a => MutableMultiSet (Prim.PrimState m) a -> a -> Int -> m ()
mmsDeletes mms a n = MutVar.modifyMutVar' mms $ msDeletes a n

mmsDeleteAll
  :: Prim.PrimMonad m
  => Ord a => MutableMultiSet (Prim.PrimState m) a -> a -> m ()
mmsDeleteAll mms a = MutVar.modifyMutVar' mms $ msDeleteAll a

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

mmsLookupLE
  :: Prim.PrimMonad m
  => Ord a => MutableMultiSet (Prim.PrimState m) a -> a -> m (Maybe a)
mmsLookupLE mms a = MutVar.readMutVar mms Functor.<&> msLookupLE a

mmsLookupGE
  :: Prim.PrimMonad m
  => Ord a => MutableMultiSet (Prim.PrimState m) a -> a -> m (Maybe a)
mmsLookupGE mms a = MutVar.readMutVar mms Functor.<&> msLookupGE a

mmsNull :: Prim.PrimMonad m => MutableMultiSet (Prim.PrimState m) a -> m Bool
mmsNull mms = MutVar.readMutVar mms Functor.<&> msNull

mmsUnion
  :: Prim.PrimMonad m
  => Ord a
  => MutableMultiSet (Prim.PrimState m) a
  -> MutableMultiSet (Prim.PrimState m) a
  -> m ()
mmsUnion mms1 mms2 = do
  ms1 <- MutVar.readMutVar mms1
  ms2 <- MutVar.readMutVar mms2
  MutVar.modifyMutVar' mms1 $ msUnion ms2

mmsSplitLTGE
  :: Prim.PrimMonad m
  => Ord a
  => MutableMultiSet (Prim.PrimState m) a
  -> a
  -> m (MultiSet a, MultiSet a)
mmsSplitLTGE mms a = MutVar.readMutVar mms Functor.<&> msSplitLTGE a

mmsSplitLEGT
  :: Prim.PrimMonad m
  => Ord a
  => MutableMultiSet (Prim.PrimState m) a
  -> a
  -> m (MultiSet a, MultiSet a)
mmsSplitLEGT mms a = MutVar.readMutVar mms Functor.<&> msSplitLEGT a

mmsElemAtL
  :: Prim.PrimMonad m
  => Ord a => MutableMultiSet (Prim.PrimState m) a -> Int -> m (Maybe a)
mmsElemAtL mms i = MutVar.readMutVar mms Functor.<&> msElemAtL i

mmsElemAtG
  :: Prim.PrimMonad m
  => Ord a => MutableMultiSet (Prim.PrimState m) a -> Int -> m (Maybe a)
mmsElemAtG mms i = MutVar.readMutVar mms Functor.<&> msElemAtG i

------------------
-- Segment Tree --
------------------

data SegmentTree a = StBranch Int a (SegmentTree a) (SegmentTree a) | StLeaf a deriving (Show)

stRoot :: SegmentTree a -> a
stRoot (StBranch _ a _ _) = a
stRoot (StLeaf a        ) = a

-- | Vector から セグ木を構築
stFromV :: VG.Vector v a => (a -> a -> a) -> v a -> SegmentTree a
stFromV f xs
  | VG.length xs == 1
  = StLeaf $ VG.head xs
  | otherwise
  = let depth  = ceiling (logBase 2 (fromIntegral (VG.length xs))) -- rootを0としたときの，木の高さ
        (l, r) = VG.splitAt (2 ^ (depth - 1)) xs
        lst    = stFromV f l
        rst    = stFromV f r
    in  StBranch depth (f (stRoot lst) (stRoot rst)) lst rst

stUpdate :: (a -> a -> a) -> Int -> a -> SegmentTree a -> SegmentTree a
stUpdate f 0 a (StLeaf _) = StLeaf a
stUpdate _ _ _ (StLeaf _) = error "stUpdate: out of range"
stUpdate f i a (StBranch d _ l r)
  | i < 2 ^ (d - 1)
  = let l' = stUpdate f i a l in StBranch d (f (stRoot l') (stRoot r)) l' r
  | otherwise
  = let r' = stUpdate f (i - 2 ^ (d - 1)) a r
    in  StBranch d (f (stRoot l) (stRoot r')) l r'

stFold :: (a -> a -> a) -> Int -> Int -> SegmentTree a -> a
stFold f _ _ (StLeaf a) = a
stFold f lb rb (StBranch d a l r)
  | lb == 0 && rb == 2 ^ d
  = a
  | rb <= 2 ^ (d - 1)
  = stFold f lb rb l
  | lb >= 2 ^ (d - 1)
  = stFold f (lb - 2 ^ (d - 1)) (rb - 2 ^ (d - 1)) r
  | otherwise
  = let lres = stFold f lb (2 ^ (d - 1)) l
        rres = stFold f 0 (rb - 2 ^ (d - 1)) r
    in  f lres rres

------------
-- Others --
------------

head :: VG.Vector v a => v a -> Maybe a
head v = if VG.null v then Nothing else Just (v ! 0)

tail :: VG.Vector v a => v a -> Maybe (v a)
tail v = if VG.null v then Nothing else Just (VG.tail v)

uncons :: VG.Vector v a => v a -> Maybe (a, v a)
uncons v = if VG.null v then Nothing else Just (v ! 0, VG.tail v)

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
  frags = VU.create do
    v <- VUM.replicate (fromIntegral (n + 1)) True
    VUM.write v 0 False
    VUM.write v 1 False
    VU.forM_
      [2 .. floor . sqrt @Double . fromIntegral $ n]
      \i -> do
        frag <- VUM.read v i
        M.when frag $ VU.forM_ [2 * i, 3 * i .. fromIntegral n]
                               \j -> VUM.write v j False
    pure v

-- | 拡張されたユークリッドの互除法
-- | ax + by = gcd a b を解く
extendedEuc :: (Integral b) => b -> b -> (b, b)
extendedEuc a b | a >= 0 && b == 0 = (1, 0)
                | a < 0 && b == 0  = (-1, 0)
                | otherwise        = (t, s - q * t) where
  (q, r) = divMod a b
  (s, t) = extendedEuc b r
