{-# LANGUAGE FlexibleContexts #-}

import qualified Data.Vector as V

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

import Control.Monad.State

type Variable = Int

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
    bnetVariables :: IntMap VariableDescription
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
addFactor f = state $ \net -> ((), net { bnetFactors = f : bnetFactors net })

unsafeSqueezeIndices :: BayesNetwork -> [Variable] -> [Variable] -> Int -> Int
unsafeSqueezeIndices net = squeeze
  where
    variables = bnetVariables net
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

unsafeTransposeIndices :: BayesNetwork -> [Variable] -> [Variable] -> Int -> Int
unsafeTransposeIndices net vars1 vars2 = \x -> sum $ zipWith (*) (getMatrixIndex x) offsets1
  where
    variables = bnetVariables net
    getDim var = vardescDim $ variables IntMap.! var
    getMatrixIndex x = tail . map snd . scanl (divMod . fst) (x, 0) . map getDim $ vars1
    offsets2 = scanl (*) 1 . map getDim $ vars2
    offsets1 = [ x | var <- vars1, let Just x = lookup var (zip vars2 offsets2) ]

netExample :: BayesNetwork
netExample = (`execState` emptyBayesNetwork) $ do
  varA <- newVariable $ VariableDescription 3
  varB <- newVariable $ VariableDescription 2
  addFactor $ Factor [varA, varB] (V.fromList [0.1, 0.2, 0.7, 0.4, 0.6, 0.0])

main = return ()
