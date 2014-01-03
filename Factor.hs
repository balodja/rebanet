module Factor where

import Prelude hiding (product)
import qualified Prelude as P
import Control.Monad (forM_)

import Data.Semiring

import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as VM
import Control.Monad.ST

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

import Data.List (sort, union, (\\))

type Variable = Int
type VariableMap = IntMap VariableDescription

data Factor a = Factor {
    factorVariables :: [Variable]
  , factorData :: V.Vector a
  } deriving (Eq, Show, Read)

data VariableDescription = VariableDescription {
    vardescDim :: !Int
--  , vardescName :: String
--  , vardescValues :: [String]
  } deriving (Eq, Show, Read)

unsafeSqueezeIndices :: VariableMap -> [Variable] -> [Variable] -> Int -> Int
unsafeSqueezeIndices variables = squeeze
  where
    getDim var = vardescDim $ variables IntMap.! var
    squeezeError = error "unsafeSqueezeIndices: vars2 is not subset of vars1"

    squeeze :: [Variable] -> [Variable] -> Int -> Int
    squeeze vars1@(v1 : vs1) vars2@(v2 : vs2)
      | v1 == v2 = let dim = getDim v1
                       acc = squeeze vs1 vs2
                   in (\x -> let (d, r) = x `divMod` dim
                             in acc d * dim + r)
      | v1 < v2 = let dim = getDim v1
                      acc = squeeze vs1 vars2
                  in \x -> acc (x `div` dim)
      | otherwise = squeezeError
    squeeze (v1 : vs1) [] = let dim = getDim v1
                                acc = squeeze vs1 []
                              in \x -> acc (x `div` dim)
    squeeze [] (v2 : vs2) = squeezeError
    squeeze [] [] = id

unsafeTransposeIndices :: VariableMap -> [Variable] -> [Variable] -> Int -> Int
unsafeTransposeIndices vmap varsFrom varsTo =
  \x -> sum $ zipWith (*) (getMatrixIndex x) offsetsFrom
  where
    getDim var = vardescDim $ vmap IntMap.! var
    getMatrixIndex x = tail . map snd . scanl (divMod . fst) (x, 0) . map getDim $ varsFrom
    offsetsTo = scanl (*) 1 . map getDim $ varsTo
    offsetsFrom = [ x | var <- varsFrom, let Just x = lookup var (zip varsTo offsetsTo) ]

unsafeTransposeFactor :: V.Unbox a => VariableMap -> Factor a -> [Variable] -> Factor a
unsafeTransposeFactor vmap factor vars =
  let getIndex = unsafeTransposeIndices vmap vars (factorVariables factor)
      datum = factorData factor
  in Factor vars (V.generate (V.length datum) $ (V.!) datum . getIndex)

normalizeFactorOrder :: V.Unbox a => VariableMap -> Factor a -> Factor a
normalizeFactorOrder vmap f =
  let vars = factorVariables f
      revars = sort vars
  in if revars == vars
     then f else unsafeTransposeFactor vmap f revars

{-# INLINE marginalize #-}
marginalize :: (V.Unbox a, Semiring a) => VariableMap -> [Variable] -> Factor a -> Factor a
marginalize vmap diffvars f =
      -- special difference for ascending lists would be a better choice
  let vars = factorVariables f \\ diffvars
      squeezeIndex = unsafeSqueezeIndices vmap (factorVariables f) vars
      olddata = factorData f
      newdata = runST $ do
        vec <- VM.unsafeNew $ P.product [ vardescDim $ vmap IntMap.! v | v <- vars ]
        VM.set vec zero
        forM_ [0 .. V.length olddata - 1] $ \oldI -> do
          let newI = squeezeIndex oldI
          x <- VM.unsafeRead vec newI
          VM.unsafeWrite vec newI (x <+> olddata V.! oldI)
        V.unsafeFreeze vec
  in Factor vars newdata

unsafeExpand :: V.Unbox a => VariableMap -> [Variable] -> Factor a -> Factor a
unsafeExpand vmap allvars f =
  let getIndex = unsafeSqueezeIndices vmap allvars (factorVariables f)
      size = P.product [ vardescDim $ vmap IntMap.! v | v <- allvars ]
  in Factor allvars (V.generate size $ (V.!) (factorData f) . getIndex)

{-# INLINE product #-}
product :: (V.Unbox a, Semiring a) => VariableMap -> [Factor a] -> Factor a
product vmap factors =
  let vars = sort . foldl union [] . map factorVariables $ factors
      vectors = map (factorData . unsafeExpand vmap vars) $ factors
  in case length factors of
    0 -> Factor [] (V.singleton one)
    1 -> head factors
    _ -> Factor vars $ foldl1 (V.zipWith (<*>)) vectors
