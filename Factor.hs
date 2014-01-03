{-# LANGUAGE FlexibleContexts, NoImplicitPrelude #-}

import Prelude hiding (product)
import qualified Prelude as P

import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import Control.Monad.ST

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

import Data.List (sort, union, (\\))

import Control.Monad.State

type Variable = Int
type VariableMap = IntMap VariableDescription

data Factor = Factor {
    factorVariables :: [Variable]
  , factorData :: V.Vector Double
  } deriving (Eq, Show, Read)

data VariableDescription = VariableDescription {
    vardescDim :: !Int
--  , vardescName :: String
--  , vardescValues :: [String]
  } deriving (Eq, Show, Read)

data BayesNetwork = BayesNetwork {
    bnetVariables :: VariableMap
  , bnetFactors :: [Factor]
  } deriving (Eq, Show, Read)

emptyBayesNetwork = BayesNetwork IntMap.empty []

newVariable :: MonadState BayesNetwork m => VariableDescription -> m Variable
newVariable desc = state modifyNet
  where
    modifyNet net = let variables = bnetVariables net
                        var = succ . foldl max 0 . IntMap.keys $ variables
                    in (var, net { bnetVariables = IntMap.insert var desc variables })

addFactor :: MonadState BayesNetwork m => Factor -> m ()
addFactor factor = do
  net <- get
  let vars = factorVariables factor
      vmap = bnetVariables net
      size = P.product [ vardescDim $ vmap IntMap.! v | v <- vars ]
  if (size == V.length (factorData factor))
    then put net { bnetFactors = normalizeFactorOrder vmap factor : bnetFactors net }
    else error "addFactor: wrong vector length"

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

unsafeTransposeFactor :: VariableMap -> Factor -> [Variable] -> Factor
unsafeTransposeFactor vmap factor vars =
  let getIndex = unsafeTransposeIndices vmap vars (factorVariables factor)
      datum = factorData factor
  in Factor vars (V.generate (V.length datum) $ (V.!) datum . getIndex)

normalizeFactorOrder :: VariableMap -> Factor -> Factor
normalizeFactorOrder vmap f =
  let vars = factorVariables f
      revars = sort vars
  in if revars == vars
     then f else unsafeTransposeFactor vmap f revars

marginalize :: VariableMap -> [Variable] -> Factor -> Factor
marginalize vmap diffvars f =
      -- special difference for ascending lists would be a better choice
  let vars = factorVariables f \\ diffvars
      squeezeIndex = unsafeSqueezeIndices vmap (factorVariables f) vars
      olddata = factorData f
      newdata = runST $ do
        vec <- VM.unsafeNew $ P.product [ vardescDim $ vmap IntMap.! v | v <- vars ]
        VM.set vec 0
        forM_ [0 .. V.length olddata - 1] $ \oldI -> do
          let newI = squeezeIndex oldI
          x <- VM.unsafeRead vec newI
          VM.unsafeWrite vec newI (x + olddata V.! oldI)
        V.unsafeFreeze vec
  in Factor vars newdata

unsafeExpand :: VariableMap -> [Variable] -> Factor -> Factor
unsafeExpand vmap allvars f =
  let getIndex = unsafeSqueezeIndices vmap allvars (factorVariables f)
      size = P.product [ vardescDim $ vmap IntMap.! v | v <- allvars ]
  in Factor allvars (V.generate size $ (V.!) (factorData f) . getIndex)

product :: VariableMap -> [Factor] -> Factor
product vmap factors =
  let vars = sort . foldl union [] . map factorVariables $ factors
      vectors = map (factorData . unsafeExpand vmap vars) $ factors
  in case length factors of
    0 -> Factor [] (V.singleton 1)
    1 -> head factors
    _ -> Factor vars $ foldl1 (V.zipWith (*)) vectors

netExample :: BayesNetwork
netExample = (`execState` emptyBayesNetwork) $ do
  varA <- newVariable $ VariableDescription 2
  varB <- newVariable $ VariableDescription 3
  addFactor $ Factor [] (V.singleton 1)
  addFactor $ Factor [varB, varA] (V.fromList [0.1, 0.2, 0.7, 0.4, 0.6, 0.0])

main = return ()
