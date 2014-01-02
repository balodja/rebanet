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

netExample :: BayesNetwork
netExample = (`execState` emptyBayesNetwork) $ do
  varA <- newVariable $ VariableDescription 3
  varB <- newVariable $ VariableDescription 2
  addFactor $ Factor [varA, varB] (V.fromList [0.1, 0.2, 0.7, 0.4, 0.6, 0.0])

main = return ()
