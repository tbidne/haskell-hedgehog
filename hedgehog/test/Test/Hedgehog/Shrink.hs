{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Hedgehog.Shrink where

import qualified Control.Concurrent as CC
import           Control.Monad (when)
import           Control.Monad.IO.Class (MonadIO(..))
import           Data.IORef (IORef)
import qualified Data.IORef as IORef

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Hedgehog.Internal.Config as Config
import qualified Hedgehog.Internal.Property as Property
import qualified Hedgehog.Internal.Runner as Runner
import           Hedgehog.Internal.Report (FailureReport(..), FailedAnnotation (..))
import           Hedgehog.Internal.Report (Report(..), Result(..))

-- No limit fully shrinks (18)
prop_ShrinkNoLimit :: Property
prop_ShrinkNoLimit =
  withTests 1 . property $ do
    (report, gens) <- checkModProp id
    [50, 0, 25, 13, 7, 4, 6, 5] === gens
    case reportStatus report of
      Failed f -> failureShrinks f === 5
      _ -> failure

-- Shrinks 3 times
prop_ShrinkLimit :: Property
prop_ShrinkLimit =
  withTests 1 . property $ do
    (report, gens) <- checkModProp (withShrinks 3)
    [50, 0, 25, 13, 7] === gens
    case reportStatus report of
      Failed f -> failureShrinks f === 3
      _ -> failure

-- Time limit of 0 i.e. does not shrink at all
prop_ShrinkTimeLimitZero :: Property
prop_ShrinkTimeLimitZero =
  withTests 1 . property $ do
    (report, gens) <- checkModProp (withShrinkTime 0)
    [50] === gens
    case reportStatus report of
      GaveUp -> pure ()
      _ -> failure

-- Time limit of 1,000,000 microseconds = 1 s. Verifies that we get a
-- "partial" shrink.
--
-- There is tension in the shrinkTime. On the one hand, we want it long enough
-- so that we generate the four values [50, 0, 25, 13] before it gets stuck
-- on 13. We don't want it too long, though, because that makes the test
-- slower.
--
-- Experience shows that values under 1 second would cause an occasional CI
-- failure (the machine would not generate all four values before the timeout).
-- A timeout of 1 second, on the other hand, passed CI with 10,000 tests
-- (and took 7 hours!). Thus we use the 1 second timeout as it seems robust
-- enough to not cause CI failures, and we cap the tests at 1 to keep the
-- running time fast.
prop_ShrinkTimeLimit :: Property
prop_ShrinkTimeLimit =
  withTests 1 . property $ do
    -- Test generates [ 50 , 0 , 25 , 13 , 7 , 4 , 6 , 5 ]
    -- The 1 s timeout combined with the 10 s delay on 13 means
    -- shrinking will get stuck on 13, hence:
    --   - only generate [50 , 0 , 25 , 13]
    --   - final shrink value is 25
    (report, gens) <- checkModPropGen delay (withShrinkTime shrinkTime)
    [50 , 0 , 25 , 13] === gens
    case reportStatus report of
      Failed f -> do
        1 === failureShrinks f
        case failureAnnotations f of
          [ann] -> "25" === failedValue ann
          _ -> failure
      _ -> failure
  where
    delay x = when (x == 13) (liftIO $ CC.threadDelay delayTime)
    shrinkTime = 1000000 --  1 sec
    -- Does not matter what this is, as long as it is longer than shrinkTime
    delayTime = 10000000 -- 10 sec

-- Given a property modifier, returns the property's report and generated
-- values.
checkModProp ::
  ( MonadIO m
  , MonadTest m
  )
  => -- property modifier
     (Property -> Property)
  -> m (Report Result, [Int])
checkModProp = checkModPropGen (const (pure ()))

-- checkModProp with function to run on the generated values
checkModPropGen ::
  ( MonadIO m
  , MonadTest m
  )
  => -- function to run on generated values
     (Int -> PropertyT IO ())
     -- property modifier
  -> (Property -> Property)
  -> m (Report Result, [Int])
checkModPropGen onGen md = do
  gensRef <- liftIO $ IORef.newIORef []
  report <- checkProp $ modProp onGen gensRef md
  gens <- liftIO $ reverse <$> IORef.readIORef gensRef
  annotateShow report
  annotateShow gens
  pure (report, gens)

modProp ::
     -- function to run on generated values
     (Int -> PropertyT IO ())
     -- reference to hold generated values
  -> IORef [Int]
     -- property modifier
  -> (Property -> Property)
  -> Property
modProp onGen gensRef md = withTests 1 . md . property $ do
  -- [ 50 , 0 , 25 , 13 , 7 , 4 , 6 , 5 ]
  x :: Int <- forAll $ Gen.integral (Range.linearFrom 0 50 100)
  liftIO $ IORef.modifyIORef' gensRef (x :)
  onGen x
  diff x (<) 5

checkProp :: MonadIO m => Property -> m (Report Result)
checkProp prop = do
  seed <- Config.resolveSeed Nothing
  liftIO $ Runner.checkReport
    (Property.propertyConfig prop)
    0
    seed
    (Property.propertyTest prop)
    (const $ pure ())

tests :: IO Bool
tests = checkParallel $$(discover)