module Model where

import Data.IntMap hiding (foldl')
import Control.Monad.Random.Lazy hiding (fromList)
import Data.List (foldl',elemIndex)
import Data.Discrimination
import Data.Monoid
import Control.Monad.Writer


-- transaction model

type Player = Int
type Wealth = Double
type Transaction m = ((Wealth , Wealth) -> m (Wealth , Wealth))

type World = IntMap Wealth

data Report = Report {
    startGini :: Double
  , endGini :: Double
  , richPercentiles :: [ (Double , Double) ]
  }

instance Show Report where
  show (Report sg eg ps) = execWriter $ do
    putLine $ "Start Gini: " <> show sg
    putLine $ "End Gini: " <> show eg
    putLine $ "Richest players' starting -> ending percentiles:"

    _ <- flip traverse ps $ \(start,end) ->
      putLine $ "    " <> showPerc start <> " -> " <> showPerc end

    pure ()

    where
      putLine :: String -> Writer String ()
      putLine s = tell $ s <> "\n"

      showPerc x =
        let percentile :: Int = round $ (*) 100 x in
        show percentile

data SimulationSetup m = SimulationSetup {
    playerCount :: Int
  , transaction :: Transaction m
  , iterationCount :: Int
  , startingWealth :: m Wealth
  }

reportSimulation :: forall m . MonadRandom m => SimulationSetup m -> m Report
reportSimulation (SimulationSetup pc t iterations genWealth) = do

  initialWorld <- startWorld

  endWorld <- stepN stepTransaction iterations initialWorld

  let top5 = findTop endWorld 5

  pure $ Report {
      startGini = gini initialWorld
    , endGini = gini endWorld
    , richPercentiles = flip fmap top5 $ \x ->
        (findPercentile x initialWorld , findPercentile x endWorld)
    }

  where
    startWorld :: m World
    startWorld = do
      wealths <- traverse (const genWealth) players
      pure . fromList $ zip players wealths

    players :: [ Player ]
    players = [ 1 .. pc ]

    stepN :: MonadRandom m => (World -> m World) -> Int -> (World -> m World)
    stepN _ 0 x = pure x
    stepN f n x = f x >>= stepN f (n - 1)

    stepTransaction :: MonadRandom m => World -> m World
    stepTransaction w = do
      -- select two random players
      p1 <- uniform players
      p2 <- uniform players
      if p1 == p2 then stepTransaction w else do
        let w1 = w ! p1
        let w2 = w ! p2
        (w1',w2') <- t (w1,w2)
        pure $ insert p1 w1' $ insert p2 w2' $ w


    gini :: World -> Double
    gini w =
      let xs = snd <$> toList w in

      let ps :: [ (Wealth , Wealth) ] = (,) <$> xs <*> xs in

      foldl' (\a p -> a + abs (fst p - snd p)) 0 ps /
        (2.0 * fromIntegral (length xs) * sum xs)

    sortWorld :: World -> [ (Player , Wealth) ]
    sortWorld w = sortWith (f . snd) $ toList w
      where
        f :: Double -> Int
        f x = round $ -1 * x

    findTop :: World -> Int -> [ Player ]
    findTop w j = fmap fst $ take j $ sortWorld w

    findPercentile :: Player -> World -> Double
    findPercentile p w =
      1.0 - fromIntegral j / fromIntegral (length xs)
      where
        xs = sortWorld w
        j = maybe 100 id $ elemIndex p $ fst <$> xs

