module Model where

import Data.IntMap hiding (foldl')
import System.Random
import Control.Monad.Random.Lazy hiding (fromList)
import Data.List (foldl',elemIndex)
import Data.Discrimination
import Data.Monoid


-- transaction model

type PlayerCount = Int
type IterationCount = Int
type Player = Int
type Wealth = Double

type World = IntMap Wealth

reportSimulation ::
  PlayerCount ->
  (forall m . MonadRandom m => (Wealth , Wealth) -> m (Wealth , Wealth)) ->
  IterationCount ->
  (forall m . MonadRandom m => m Wealth)
  -> IO ()
reportSimulation playerCount t iterations genWealth = do

  putStrLn $ "Simulating " <> show playerCount <> " players at " <> show iterations <> " iterations..."

  gen <- newStdGen

  (initialWorld,g) <- runRandT startWorld gen

  endWorld <- flip evalRandT g $
    stepN stepTransaction iterations initialWorld

  putStrLn $ "Start Gini: " <> show (gini initialWorld)
  putStrLn $ "End Gini: " <> show (gini endWorld)

  let top5 = findTop endWorld 5

  putStrLn "Richest players' starting -> ending percentiles:"

  _ <- flip traverse top5 $ \x -> do

    putStrLn $ "   " <> showPerc x initialWorld <> " -> " <> showPerc x endWorld <> ""

  pure ()

  where
    showPerc x w =
      let percentile :: Int = round $ (*) 100 $ findPercentile x w in
      show percentile

    startWorld :: MonadRandom m => m World
    startWorld = do
      wealths <- traverse (const genWealth) players
      pure . fromList $ zip players wealths

    players :: [ Player ]
    players = [ 1 .. playerCount ]

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

