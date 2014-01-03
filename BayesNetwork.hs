{-# LANGUAGE FlexibleContexts #-}
module BayesNetwork where

import qualified Factor as F

import qualified Data.Vector as V

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

import Data.List (sort, union, partition, (\\))

import Control.Monad.State

data BayesNetwork = BayesNetwork {
    bnetVariables :: F.VariableMap
  , bnetFactors :: [F.Factor]
  } deriving (Eq, Show, Read)

emptyBayesNetwork = BayesNetwork IntMap.empty []

newVariable :: MonadState BayesNetwork m => F.VariableDescription -> m F.Variable
newVariable desc = state modifyNet
  where
    modifyNet net = let variables = bnetVariables net
                        var = succ . foldl max 0 . IntMap.keys $ variables
                    in (var, net { bnetVariables = IntMap.insert var desc variables })

addFactor :: MonadState BayesNetwork m => F.Factor -> m ()
addFactor factor = do
  net <- get
  let vars = F.factorVariables factor
      vmap = bnetVariables net
      size = product [ F.vardescDim $ vmap IntMap.! v | v <- vars ]
  if (size == V.length (F.factorData factor))
    then put net { bnetFactors = F.normalizeFactorOrder vmap factor : bnetFactors net }
    else error "addFactor: wrong vector length"

eliminateVariable :: MonadState BayesNetwork m => F.Variable -> m ()
eliminateVariable var = state modifyNet
  where
    modifyNet net =
      let (facs, notFacs) = partition (elem var . F.factorVariables) (bnetFactors net)
          vars = bnetVariables net
          f = F.marginalize vars [var] (F.product vars facs)
      in ((), net { bnetFactors = f : notFacs })

netExample :: BayesNetwork
netExample = (`execState` emptyBayesNetwork) $ do
  varA <- newVariable $ F.VariableDescription 2
  varB <- newVariable $ F.VariableDescription 3
  addFactor $ F.Factor [] (V.singleton 1)
  addFactor $ F.Factor [varA] (V.fromList [0.5, 0.5])
  addFactor $ F.Factor [varB, varA] (V.fromList [0.1, 0.2, 0.7, 0.4, 0.6, 0.0])
  eliminateVariable varA
