module Main where

import Control.Monad.Random.Lazy (MonadRandom(getRandom,getRandomR))
import Data.Monoid
import Model


-- various transaction functions

-- | Transaction function where both parties gamble all of their wealth
winnerTakesAll :: MonadRandom m => (Wealth , Wealth) -> m (Wealth , Wealth)
winnerTakesAll (w1 , w2) = do

  let pot = w1 + w2

  k :: Bool <- getRandom

  pure $ if k then (pot , 0) else (0 , pot)


-- | Transaction function where a random amount of wealth both parties can afford is gambled
randomLimitByNegotiatingPower :: MonadRandom m => (Wealth , Wealth) -> m (Wealth , Wealth)
randomLimitByNegotiatingPower (w1 , w2) = do

  v <- getRandomR (0.0 , min w1 w2)

  k :: Bool <- getRandom

  pure $ if k then (w1 + v , w2 - v) else (w1 - v , w2 + v)


main :: IO ()
main = do

  let setup = SimulationSetup {
      playerCount = 500
    , transaction = randomLimitByNegotiatingPower
    , iterationCount = 5000
    , startingWealth = genWealth
    }

  putStrLn $
        "Simulating "
     <> show (playerCount setup)
     <> " players at "
     <> show (iterationCount setup)
     <> " iterations..."
  
  reportSimulation setup >>= print

  where
    genWealth :: MonadRandom m => m Wealth
    genWealth = do
      -- uniform wealth scale (low start gini)
      -- getRandomR (1000 , 2000000)

      -- exponential wealth scale (high start gini)
      v :: Double <- getRandomR (0.0 , 6.0)
      pure $ 1000 * 2.7 ** v

