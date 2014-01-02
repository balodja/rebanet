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

unsafeTranslateIndices :: BayesNetwork -> [Variable] -> [Variable] -> Int -> Int
unsafeTranslateIndices net = translate
  where
    variables = bnetVariables net
    getDim var = vardescDim $ variables IntMap.! var
    translateError = error "unsafeTranslateIndices: vars2 is not subset of vars1"

    translate :: [Variable] -> [Variable] -> Int -> Int
    translate vars1@(v1 : vs1) vars2@(v2 : vs2)
      | v1 == v2 = let dim = getDim v1
                       acc = translate vs1 vs2
                   in (\x -> let (d, r) = x `divMod` dim
                             in acc d * dim + r)
      | v1 < v2 = let dim = getDim v1
                      acc = translate vs1 vars2
                  in \x -> acc (x `div` dim)
      | otherwise = translateError
    translate (v1 : vs1) [] = let dim = getDim v1
                                  acc = translate vs1 []
                              in \x -> acc (x `div` dim)
    translate [] (v2 : vs2) = translateError
    translate [] [] = id

netExample :: BayesNetwork
netExample = (`execState` emptyBayesNetwork) $ do
  varA <- newVariable $ VariableDescription 3
  varB <- newVariable $ VariableDescription 2
  addFactor $ Factor [varA, varB] (V.fromList [0.1, 0.2, 0.7, 0.4, 0.6, 0.0])

main = return ()
