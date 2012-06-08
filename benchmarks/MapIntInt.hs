{-# LANGUAGE BangPatterns #-}
module Main where

import Control.DeepSeq
import Control.Exception (evaluate)
import Control.Monad.Trans (liftIO)
import Criterion.Config
import Criterion.Main
import Data.List (foldl')
import qualified Data.Map as MG
import qualified Data.Map.BaseIntInt as MI
import qualified Data.IntMap as MH
import qualified Data.IntMap.Strict as MHS
import qualified Data.IntMap.BaseInt as MF
import Data.Maybe (fromMaybe)
import Prelude hiding (lookup)

main = do
    let mg = MG.fromAscList elems :: MG.Map Int Int
        mg_even = MG.fromAscList elems_even :: MG.Map Int Int
        mg_odd = MG.fromAscList elems_odd :: MG.Map Int Int
    let mi = MI.fromAscList elems :: MI.MapIntInt
        mi_even = MI.fromAscList elems_even :: MI.MapIntInt
        mi_odd = MI.fromAscList elems_odd :: MI.MapIntInt
    let mh = MH.fromAscList elems :: MH.IntMap Int
        mh_even = MH.fromAscList elems_even :: MH.IntMap Int
        mh_odd = MH.fromAscList elems_odd :: MH.IntMap Int
    let mf = MF.fromAscList elems :: MF.IntMapInt
        mf_even = MF.fromAscList elems_even :: MF.IntMapInt
        mf_odd = MF.fromAscList elems_odd :: MF.IntMapInt
    defaultMainWith
        defaultConfig
        (liftIO $ do
            evaluate $ rnf [mg, mg_even, mg_odd]
            evaluate $ rnf [mi, mi_even, mi_odd]
            evaluate $ rnf [mh, mh_even, mh_odd]
        )
        [ bgroup "lookup absent"
           [ bench "generic" $ nf (g_lookup evens) mg_odd
           , bench "unboxed" $ nf (i_lookup evens) mi_odd
           , bench "gintmap" $ nf (h_lookup evens) mh_odd
           , bench "uintmap" $ nf (f_lookup evens) mf_odd ]
        , bgroup "lookup present"
           [ bench "generic" $ nf (g_lookup evens) mg_even
           , bench "unboxed" $ nf (i_lookup evens) mi_even
           , bench "gintmap" $ nf (h_lookup evens) mh_even
           , bench "uintmap" $ nf (f_lookup evens) mf_even ]
        , bgroup "insert absent"
           [ bench "generic" $ nf (g_ins elems_even) mg_odd
           , bench "unboxed" $ nf (i_ins elems_even) mi_odd
           , bench "gintmap" $ nf (h_ins elems_even) mh_odd
           , bench "uintmap" $ nf (f_ins elems_even) mf_odd ]
        , bgroup "insert present"
           [ bench "generic" $ nf (g_ins elems_even) mg_even
           , bench "unboxed" $ nf (i_ins elems_even) mi_even
           , bench "gintmap" $ nf (h_ins elems_even) mh_even
           , bench "uintmap" $ nf (f_ins elems_even) mf_even ]
        , bgroup "insertWith absent"
           [ bench "generic" $ nf (g_insWith elems_even) mg_odd
           , bench "unboxed" $ nf (i_insWith elems_even) mi_odd
           , bench "gintmap" $ nf (h_insWith elems_even) mh_odd
           , bench "uintmap" $ nf (f_insWith elems_even) mf_odd ]
        , bgroup "insertWith present"
           [ bench "generic" $ nf (g_insWith elems_even) mg_even
           , bench "unboxed" $ nf (i_insWith elems_even) mi_even
           , bench "gintmap" $ nf (h_insWith elems_even) mh_even
           , bench "uintmap" $ nf (f_insWith elems_even) mf_even ]
        , bgroup "insertWith' absent"
           [ bench "generic" $ nf (g_insWith' elems_even) mg_odd
           , bench "unboxed" $ nf (i_insWith' elems_even) mi_odd
           , bench "gintmap" $ nf (h_insWith' elems_even) mh_odd
           , bench "uintmap" $ nf (f_insWith' elems_even) mf_odd ]
        , bgroup "insertWith' present"
           [ bench "generic" $ nf (g_insWith' elems_even) mg_even
           , bench "unboxed" $ nf (i_insWith' elems_even) mi_even
           , bench "gintmap" $ nf (h_insWith' elems_even) mh_even
           , bench "uintmap" $ nf (f_insWith' elems_even) mf_even ]
        , bgroup "insertWithKey absent"
           [ bench "generic" $ nf (g_insWithKey elems_even) mg_odd
           , bench "unboxed" $ nf (i_insWithKey elems_even) mi_odd
           , bench "gintmap" $ nf (h_insWithKey elems_even) mh_odd
           , bench "uintmap" $ nf (f_insWithKey elems_even) mf_odd ]
        , bgroup "insertWithKey present"
           [ bench "generic" $ nf (g_insWithKey elems_even) mg_even
           , bench "unboxed" $ nf (i_insWithKey elems_even) mi_even
           , bench "gintmap" $ nf (h_insWithKey elems_even) mh_even
           , bench "uintmap" $ nf (f_insWithKey elems_even) mf_even ]
        , bgroup "insertWithKey' absent"
           [ bench "generic" $ nf (g_insWithKey' elems_even) mg_odd
           , bench "unboxed" $ nf (i_insWithKey' elems_even) mi_odd
           , bench "gintmap" $ nf (h_insWithKey' elems_even) mh_odd
           , bench "uintmap" $ nf (f_insWithKey' elems_even) mf_odd ]
        , bgroup "insertWithKey' present"
           [ bench "generic" $ nf (g_insWithKey' elems_even) mg_even
           , bench "unboxed" $ nf (i_insWithKey' elems_even) mi_even
           , bench "gintmap" $ nf (h_insWithKey' elems_even) mh_even
           , bench "uintmap" $ nf (f_insWithKey' elems_even) mf_even ]
        , bgroup "insertLookupWithKey absent"
           [ bench "generic" $ nf (g_insLookupWithKey elems_even) mg_odd
           , bench "unboxed" $ nf (i_insLookupWithKey elems_even) mi_odd
           , bench "gintmap" $ nf (h_insLookupWithKey elems_even) mh_odd
           , bench "uintmap" $ nf (f_insLookupWithKey elems_even) mf_odd ]
        , bgroup "insertLookupWithKey present"
           [ bench "generic" $ nf (g_insLookupWithKey elems_even) mg_even
           , bench "unboxed" $ nf (i_insLookupWithKey elems_even) mi_even
           , bench "gintmap" $ nf (h_insLookupWithKey elems_even) mh_even
           , bench "uintmap" $ nf (f_insLookupWithKey elems_even) mf_even ]
        , bgroup "insertLookupWithKey' absent"
           [ bench "generic" $ nf (g_insLookupWithKey' elems_even) mg_odd
           , bench "unboxed" $ nf (i_insLookupWithKey' elems_even) mi_odd
           , bench "gintmap" $ nf (h_insLookupWithKey' elems_even) mh_odd
           , bench "uintmap" $ nf (f_insLookupWithKey' elems_even) mf_odd ]
        , bgroup "insertLookupWithKey' present"
           [ bench "generic" $ nf (g_insLookupWithKey' elems_even) mg_even
           , bench "unboxed" $ nf (i_insLookupWithKey' elems_even) mi_even
           , bench "gintmap" $ nf (h_insLookupWithKey' elems_even) mh_even
           , bench "uintmap" $ nf (f_insLookupWithKey' elems_even) mf_even ]
        , bgroup "map"
           [ bench "generic" $ nf (MG.map (+ 1)) mg
           , bench "unboxed" $ nf (MI.map (+ 1)) mi
           , bench "gintmap" $ nf (MH.map (+ 1)) mh
           , bench "uintmap" $ nf (MF.map (+ 1)) mf ]
        , bgroup "mapWithKey"
           [ bench "generic" $ nf (MG.mapWithKey (+)) mg
           , bench "unboxed" $ nf (MI.mapWithKey (+)) mi
           , bench "gintmap" $ nf (MH.mapWithKey (+)) mh
           , bench "uintmap" $ nf (MF.mapWithKey (+)) mf ]
        , bgroup "foldlWithKey"
           [ bench "generic" $ nf (MG.foldlWithKey' sum 0) mg
           , bench "unboxed" $ nf (MI.foldlWithKey' sum 0) mi
           , bench "gintmap" $ nf (MH.foldlWithKey' sum 0) mh
           , bench "uintmap" $ nf (MF.foldlWithKey' sum 0) mf ]
        , bgroup "foldrWithKey"
           [ bench "generic" $ nf (MG.foldrWithKey consPair []) mg
           , bench "unboxed" $ nf (MI.foldrWithKey consPair []) mi
           , bench "gintmap" $ nf (MH.foldrWithKey consPair []) mh
           , bench "uintmap" $ nf (MF.foldrWithKey consPair []) mf ]
        , bgroup "delete absent"
           [ bench "generic" $ nf (g_del evens) mg_odd
           , bench "unboxed" $ nf (i_del evens) mi_odd
           , bench "gintmap" $ nf (h_del evens) mh_odd
           , bench "uintmap" $ nf (f_del evens) mf_odd ]
        , bgroup "delete present"
           [ bench "generic" $ nf (g_del evens) mg
           , bench "unboxed" $ nf (i_del evens) mi
           , bench "gintmap" $ nf (h_del evens) mh
           , bench "uintmap" $ nf (f_del evens) mf ]
        , bgroup "update absent"
           [ bench "generic" $ nf (g_upd Just evens) mg_odd
           , bench "unboxed" $ nf (i_upd Just evens) mi_odd
           , bench "gintmap" $ nf (h_upd Just evens) mh_odd
           , bench "uintmap" $ nf (f_upd Just evens) mf_odd ]
        , bgroup "update present"
           [ bench "generic" $ nf (g_upd Just evens) mg_even
           , bench "unboxed" $ nf (i_upd Just evens) mi_even
           , bench "gintmap" $ nf (h_upd Just evens) mh_even
           , bench "uintmap" $ nf (f_upd Just evens) mf_even ]
        , bgroup "update delete"
           [ bench "generic" $ nf (g_upd (const Nothing) evens) mg
           , bench "unboxed" $ nf (i_upd (const Nothing) evens) mi
           , bench "gintmap" $ nf (h_upd (const Nothing) evens) mh
           , bench "uintmap" $ nf (f_upd (const Nothing) evens) mf ]
        , bgroup "updateLookupWithKey absent"
           [ bench "generic" $ nf (g_upd' Just evens) mg_odd
           , bench "unboxed" $ nf (i_upd' Just evens) mi_odd
           , bench "gintmap" $ nf (h_upd' Just evens) mh_odd
           , bench "uintmap" $ nf (f_upd' Just evens) mf_odd ]
        , bgroup "updateLookupWithKey present"
           [ bench "generic" $ nf (g_upd' Just evens) mg_even
           , bench "unboxed" $ nf (i_upd' Just evens) mi_even
           , bench "gintmap" $ nf (h_upd' Just evens) mh_even
           , bench "uintmap" $ nf (f_upd' Just evens) mf_even ]
        , bgroup "updateLookupWithKey delete"
           [ bench "generic" $ nf (g_upd' (const Nothing) evens) mg
           , bench "unboxed" $ nf (i_upd' (const Nothing) evens) mi
           , bench "gintmap" $ nf (h_upd' (const Nothing) evens) mh
           , bench "uintmap" $ nf (f_upd' (const Nothing) evens) mf ]
        , bgroup "alter absent"
           [ bench "generic" $ nf (g_alt id evens) mg_odd
           , bench "unboxed" $ nf (i_alt id evens) mi_odd
           , bench "gintmap" $ nf (h_alt id evens) mh_odd
           , bench "uintmap" $ nf (f_alt id evens) mf_odd ]
        , bgroup "alter insert"
           [ bench "generic" $ nf (g_alt (const (Just 1)) evens) mg_odd
           , bench "unboxed" $ nf (i_alt (const (Just 1)) evens) mi_odd
           , bench "gintmap" $ nf (h_alt (const (Just 1)) evens) mh_odd
           , bench "uintmap" $ nf (f_alt (const (Just 1)) evens) mf_odd ]
        , bgroup "alter update"
           [ bench "generic" $ nf (g_alt id evens) mg_even
           , bench "unboxed" $ nf (i_alt id evens) mi_even
           , bench "gintmap" $ nf (h_alt id evens) mh_even
           , bench "uintmap" $ nf (f_alt id evens) mf_even ]
        , bgroup "alter delete"
           [ bench "generic" $ nf (g_alt (const Nothing) evens) mg
           , bench "unboxed" $ nf (i_alt (const Nothing) evens) mi
           , bench "gintmap" $ nf (h_alt (const Nothing) evens) mh
           , bench "uintmap" $ nf (f_alt (const Nothing) evens) mf ]
        , bgroup "mapMaybe"
           [ bench "generic" $ nf (MG.mapMaybe maybeDel) mg
           , bench "unboxed" $ nf (MI.mapMaybe maybeDel) mi
           , bench "gintmap" $ nf (MH.mapMaybe maybeDel) mh
           , bench "uintmap" $ nf (MF.mapMaybe maybeDel) mf ]
        , bgroup "mapMaybeWithKey"
           [ bench "generic" $ nf (MG.mapMaybeWithKey (const maybeDel)) mg
           , bench "unboxed" $ nf (MI.mapMaybeWithKey (const maybeDel)) mi
           , bench "gintmap" $ nf (MH.mapMaybeWithKey (const maybeDel)) mh
           , bench "uintmap" $ nf (MF.mapMaybeWithKey (const maybeDel)) mf ]
        , bgroup "lookupIndex"
           [ bench "generic" $ nf (g_lookupIndex keys) mg
           , bench "unboxed" $ nf (i_lookupIndex keys) mi
           {-, bench "gintmap" $ nf (h_lookupIndex keys) mh
           , bench "uintmap" $ nf (f_lookupIndex keys) mf-} ]
        , bgroup "union"
           [ bench "generic" $ nf (MG.union mg_even) mg_odd
           , bench "unboxed" $ nf (MI.union mi_even) mi_odd
           , bench "gintmap" $ nf (MH.union mh_even) mh_odd
           , bench "uintmap" $ nf (MF.union mf_even) mf_odd ]
        , bgroup "difference"
           [ bench "generic" $ nf (MG.difference mg) mg_even
           , bench "unboxed" $ nf (MI.difference mi) mi_even
           , bench "gintmap" $ nf (MH.difference mh) mh_even
           , bench "uintmap" $ nf (MF.difference mf) mf_even ]
        , bgroup "intersection"
           [ bench "generic" $ nf (MG.intersection mg) mg_even
           , bench "unboxed" $ nf (MI.intersection mi) mi_even
           , bench "gintmap" $ nf (MH.intersection mh) mh_even
           , bench "uintmap" $ nf (MF.intersection mf) mf_even ]
        ]
  where
    bound = 2^10
    elems = zip keys values
    elems_even = zip evens evens
    elems_odd = zip odds odds
    keys = [1..bound]
    evens = [2,4..bound]
    odds = [1,3..bound]
    values = [1..bound]
    sum k v1 v2 = k + v1 + v2
    consPair k v xs = (k, v) : xs

add3 :: Int -> Int -> Int -> Int
add3 x y z = x + y + z
{-# INLINE add3 #-}

maybeDel :: Int -> Maybe Int
maybeDel n | n `mod` 3 == 0 = Nothing
           | otherwise      = Just n

data PairS a b = PS !a !b

--------------------------------------------------------------------------------
-- MG : generic
--------------------------------------------------------------------------------

g_lookup :: [Int] -> MG.Map Int Int -> Int
g_lookup xs m = foldl' (\n k -> fromMaybe n (MG.lookup k m)) 0 xs

g_lookupIndex :: [Int] -> MG.Map Int Int -> Int
g_lookupIndex xs m = foldl' (\n k -> fromMaybe n (MG.lookupIndex k m)) 0 xs

g_ins :: [(Int, Int)] -> MG.Map Int Int -> MG.Map Int Int
g_ins xs m = foldl' (\m (k, v) -> MG.insert k v m) m xs

g_insWith :: [(Int, Int)] -> MG.Map Int Int -> MG.Map Int Int
g_insWith xs m = foldl' (\m (k, v) -> MG.insertWith (+) k v m) m xs

g_insWithKey :: [(Int, Int)] -> MG.Map Int Int -> MG.Map Int Int
g_insWithKey xs m = foldl' (\m (k, v) -> MG.insertWithKey add3 k v m) m xs

g_insWith' :: [(Int, Int)] -> MG.Map Int Int -> MG.Map Int Int
g_insWith' xs m = foldl' (\m (k, v) -> MG.insertWith' (+) k v m) m xs

g_insWithKey' :: [(Int, Int)] -> MG.Map Int Int -> MG.Map Int Int
g_insWithKey' xs m = foldl' (\m (k, v) -> MG.insertWithKey' add3 k v m) m xs

g_insLookupWithKey :: [(Int, Int)] -> MG.Map Int Int -> (Int, MG.Map Int Int)
g_insLookupWithKey xs m = let !(PS a b) = foldl' f (PS 0 m) xs in (a, b)
  where
    f (PS n m) (k, v) = let !(n', m') = MG.insertLookupWithKey add3 k v m
                        in PS (fromMaybe 0 n' + n) m'

g_insLookupWithKey' :: [(Int, Int)] -> MG.Map Int Int -> (Int, MG.Map Int Int)
g_insLookupWithKey' xs m = let !(PS a b) = foldl' f (PS 0 m) xs in (a, b)
  where
    f (PS n m) (k, v) = let !(n', m') = MG.insertLookupWithKey' add3 k v m
                        in PS (fromMaybe 0 n' + n) m'

g_del :: [Int] -> MG.Map Int Int -> MG.Map Int Int
g_del xs m = foldl' (\m k -> MG.delete k m) m xs

g_upd :: (Int -> Maybe Int) -> [Int] -> MG.Map Int Int -> MG.Map Int Int
g_upd f xs m = foldl' (\m k -> MG.update f k m) m xs

g_upd' :: (Int -> Maybe Int) -> [Int] -> MG.Map Int Int -> MG.Map Int Int
g_upd' f xs m = foldl' (\m k -> snd $ MG.updateLookupWithKey (\_ a -> f a) k m) m xs

g_alt :: (Maybe Int -> Maybe Int) -> [Int] -> MG.Map Int Int -> MG.Map Int Int
g_alt f xs m = foldl' (\m k -> MG.alter f k m) m xs

--------------------------------------------------------------------------------
-- MI : unboxed / specialized
--------------------------------------------------------------------------------

i_lookup :: [Int] -> MI.MapIntInt -> Int
i_lookup xs m = foldl' (\n k -> fromMaybe n (MI.lookup k m)) 0 xs

i_lookupIndex :: [Int] -> MI.MapIntInt -> Int
i_lookupIndex xs m = foldl' (\n k -> fromMaybe n (MI.lookupIndex k m)) 0 xs

i_ins :: [(Int, Int)] -> MI.MapIntInt -> MI.MapIntInt
i_ins xs m = foldl' (\m (k, v) -> MI.insert k v m) m xs

i_insWith :: [(Int, Int)] -> MI.MapIntInt -> MI.MapIntInt
i_insWith xs m = foldl' (\m (k, v) -> MI.insertWith (+) k v m) m xs

i_insWithKey :: [(Int, Int)] -> MI.MapIntInt -> MI.MapIntInt
i_insWithKey xs m = foldl' (\m (k, v) -> MI.insertWithKey add3 k v m) m xs

i_insWith' :: [(Int, Int)] -> MI.MapIntInt -> MI.MapIntInt
i_insWith' xs m = foldl' (\m (k, v) -> MI.insertWith' (+) k v m) m xs

i_insWithKey' :: [(Int, Int)] -> MI.MapIntInt -> MI.MapIntInt
i_insWithKey' xs m = foldl' (\m (k, v) -> MI.insertWithKey' add3 k v m) m xs

i_insLookupWithKey :: [(Int, Int)] -> MI.MapIntInt -> (Int, MI.MapIntInt)
i_insLookupWithKey xs m = let !(PS a b) = foldl' f (PS 0 m) xs in (a, b)
  where
    f (PS n m) (k, v) = let !(n', m') = MI.insertLookupWithKey add3 k v m
                        in PS (fromMaybe 0 n' + n) m'

i_insLookupWithKey' :: [(Int, Int)] -> MI.MapIntInt -> (Int, MI.MapIntInt)
i_insLookupWithKey' xs m = let !(PS a b) = foldl' f (PS 0 m) xs in (a, b)
  where
    f (PS n m) (k, v) = let !(n', m') = MI.insertLookupWithKey' add3 k v m
                        in PS (fromMaybe 0 n' + n) m'

i_del :: [Int] -> MI.MapIntInt -> MI.MapIntInt
i_del xs m = foldl' (\m k -> MI.delete k m) m xs

i_upd :: (Int -> Maybe Int) -> [Int] -> MI.MapIntInt -> MI.MapIntInt
i_upd f xs m = foldl' (\m k -> MI.update f k m) m xs

i_upd' :: (Int -> Maybe Int) -> [Int] -> MI.MapIntInt -> MI.MapIntInt
i_upd' f xs m = foldl' (\m k -> snd $ MI.updateLookupWithKey (\_ a -> f a) k m) m xs

i_alt :: (Maybe Int -> Maybe Int) -> [Int] -> MI.MapIntInt -> MI.MapIntInt
i_alt f xs m = foldl' (\m k -> MI.alter f k m) m xs

--------------------------------------------------------------------------------
-- MH : generic IntMap
--------------------------------------------------------------------------------

h_lookup :: [Int] -> MH.IntMap Int -> Int
h_lookup xs m = foldl' (\n k -> fromMaybe n (MH.lookup k m)) 0 xs

{-
h_lookupIndex :: [Int] -> MH.IntMap Int -> Int
h_lookupIndex xs m = foldl' (\n k -> fromMaybe n (MH.lookupIndex k m)) 0 xs
-}

h_ins :: [(Int, Int)] -> MH.IntMap Int -> MH.IntMap Int
h_ins xs m = foldl' (\m (k, v) -> MH.insert k v m) m xs

h_insWith :: [(Int, Int)] -> MH.IntMap Int -> MH.IntMap Int
h_insWith xs m = foldl' (\m (k, v) -> MH.insertWith (+) k v m) m xs

h_insWithKey :: [(Int, Int)] -> MH.IntMap Int -> MH.IntMap Int
h_insWithKey xs m = foldl' (\m (k, v) -> MH.insertWithKey add3 k v m) m xs

h_insWith' :: [(Int, Int)] -> MH.IntMap Int -> MH.IntMap Int
h_insWith' xs m = foldl' (\m (k, v) -> MH.insertWith' (+) k v m) m xs

h_insWithKey' :: [(Int, Int)] -> MH.IntMap Int -> MH.IntMap Int
h_insWithKey' xs m = foldl' (\m (k, v) -> MH.insertWithKey' add3 k v m) m xs

h_insLookupWithKey :: [(Int, Int)] -> MH.IntMap Int -> (Int, MH.IntMap Int)
h_insLookupWithKey xs m = let !(PS a b) = foldl' f (PS 0 m) xs in (a, b)
  where
    f (PS n m) (k, v) = let !(n', m') = MH.insertLookupWithKey add3 k v m
                        in PS (fromMaybe 0 n' + n) m'

h_insLookupWithKey' :: [(Int, Int)] -> MH.IntMap Int -> (Int, MH.IntMap Int)
h_insLookupWithKey' xs m = let !(PS a b) = foldl' f (PS 0 m) xs in (a, b)
  where
    f (PS n m) (k, v) = let !(n', m') = MHS.insertLookupWithKey add3 k v m
                        in PS (fromMaybe 0 n' + n) m'

h_del :: [Int] -> MH.IntMap Int -> MH.IntMap Int
h_del xs m = foldl' (\m k -> MH.delete k m) m xs

h_upd :: (Int -> Maybe Int) -> [Int] -> MH.IntMap Int -> MH.IntMap Int
h_upd f xs m = foldl' (\m k -> MH.update f k m) m xs

h_upd' :: (Int -> Maybe Int) -> [Int] -> MH.IntMap Int -> MH.IntMap Int
h_upd' f xs m = foldl' (\m k -> snd $ MH.updateLookupWithKey (\_ a -> f a) k m) m xs

h_alt :: (Maybe Int -> Maybe Int) -> [Int] -> MH.IntMap Int -> MH.IntMap Int
h_alt f xs m = foldl' (\m k -> MH.alter f k m) m xs

--------------------------------------------------------------------------------
-- MF : unboxed / specialized IntMap
--------------------------------------------------------------------------------

f_lookup :: [Int] -> MF.IntMapInt -> Int
f_lookup xs m = foldl' (\n k -> fromMaybe n (MF.lookup k m)) 0 xs

{-
f_lookupIndex :: [Int] -> MF.IntMapInt -> Int
f_lookupIndex xs m = foldl' (\n k -> fromMaybe n (MF.lookupIndex k m)) 0 xs
-}

f_ins :: [(Int, Int)] -> MF.IntMapInt -> MF.IntMapInt
f_ins xs m = foldl' (\m (k, v) -> MF.insert k v m) m xs

f_insWith :: [(Int, Int)] -> MF.IntMapInt -> MF.IntMapInt
f_insWith xs m = foldl' (\m (k, v) -> MF.insertWith (+) k v m) m xs

f_insWithKey :: [(Int, Int)] -> MF.IntMapInt -> MF.IntMapInt
f_insWithKey xs m = foldl' (\m (k, v) -> MF.insertWithKey add3 k v m) m xs

f_insWith' :: [(Int, Int)] -> MF.IntMapInt -> MF.IntMapInt
f_insWith' xs m = foldl' (\m (k, v) -> MF.insertWith' (+) k v m) m xs

f_insWithKey' :: [(Int, Int)] -> MF.IntMapInt -> MF.IntMapInt
f_insWithKey' xs m = foldl' (\m (k, v) -> MF.insertWithKey' add3 k v m) m xs

f_insLookupWithKey :: [(Int, Int)] -> MF.IntMapInt -> (Int, MF.IntMapInt)
f_insLookupWithKey xs m = let !(PS a b) = foldl' f (PS 0 m) xs in (a, b)
  where
    f (PS n m) (k, v) = let !(n', m') = MF.insertLookupWithKey add3 k v m
                        in PS (fromMaybe 0 n' + n) m'

f_insLookupWithKey' :: [(Int, Int)] -> MF.IntMapInt -> (Int, MF.IntMapInt)
f_insLookupWithKey' xs m = let !(PS a b) = foldl' f (PS 0 m) xs in (a, b)
  where
    f (PS n m) (k, v) = let !(n', m') = MF.insertLookupWithKey' add3 k v m
                        in PS (fromMaybe 0 n' + n) m'

f_del :: [Int] -> MF.IntMapInt -> MF.IntMapInt
f_del xs m = foldl' (\m k -> MF.delete k m) m xs

f_upd :: (Int -> Maybe Int) -> [Int] -> MF.IntMapInt -> MF.IntMapInt
f_upd f xs m = foldl' (\m k -> MF.update f k m) m xs

f_upd' :: (Int -> Maybe Int) -> [Int] -> MF.IntMapInt -> MF.IntMapInt
f_upd' f xs m = foldl' (\m k -> snd $ MF.updateLookupWithKey (\_ a -> f a) k m) m xs

f_alt :: (Maybe Int -> Maybe Int) -> [Int] -> MF.IntMapInt -> MF.IntMapInt
f_alt f xs m = foldl' (\m k -> MF.alter f k m) m xs

