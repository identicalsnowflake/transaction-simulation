# Transaction simulation

This is a simple transaction simulation model which operates on four inputs: number of players, a transaction function, an iteration count, and an initial wealth distributor.

To run the simulation, make sure you have Haskell installed and simply do `cabal sandbox init`, `cabal install`, and `cabal run`.

Running the simulation will report a gini for the starting and ending worlds, as well as the starting and ending percentiles of the players who ended up the most wealthy.

### Transaction function

A transaction function is a function `MonadRandom m => (Wealth , Wealth) -> m (Wealth , Wealth)`, which means it's a function in the `MonadRandom` monad which accepts two wealth inputs, one for each party to the transaction, and returns two wealth outputs, again, one for each party.

```haskell
-- example transaction function
-- | Transaction function where a random amount of wealth both parties can afford is gambled
randomLimitByNegotiatingPower :: MonadRandom m => (Wealth , Wealth) -> m (Wealth , Wealth)
randomLimitByNegotiatingPower (w1 , w2) = do

  v <- getRandomR (0.0 , min w1 w2)

  k :: Bool <- getRandom

  pure $ if k then (w1 + v , w2 - v) else (w1 - v , w2 + v)

```

### Initial wealth

Initial wealth is simply a `MonadRandom m => m Wealth`:

```haskell
genWealth :: MonadRandom m => m Wealth
genWealth =
  -- uniform wealth scale (low start gini)
  getRandomR (1000 , 2000000)

```
