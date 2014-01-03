{-# LANGUAGE FlexibleContexts #-}

import qualified Data.Vector as V

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

import Data.List (sort)

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
      size = product [ vardescDim $ vmap IntMap.! v | v <- vars ]
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

netExample :: BayesNetwork
netExample = (`execState` emptyBayesNetwork) $ do
  varA <- newVariable $ VariableDescription 2
  varB <- newVariable $ VariableDescription 3
  addFactor $ Factor [] (V.singleton 1)
  addFactor $ Factor [varB, varA] (V.fromList [0.1, 0.2, 0.7, 0.4, 0.6, 0.0])

main = return ()
