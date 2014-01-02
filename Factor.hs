import qualified Data.Vector as V
import qualified Data.IntMap as IM

type Variable = Int

data Factor = Factor {
    factorVariables :: [Variable]
  , factorData :: V.Vector Double
  } deriving (Eq, Show, Read)

data VariableDescription = VariableDescription {
    vardescDim :: !Int
  , vardescName :: String
  , vardescValues :: [String]
  } deriving (Eq, Show, Read)

data BayesNetwork = BayesNetwork {
    bnetVariables :: IM.IntMap VariableDescription
  , bnetFactors :: [Factor]
  } deriving (Eq, Show, Read)

main = putStrLn "halo"
