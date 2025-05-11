module Unit.Data.Time.Relative
  ( tests,
  )
where

import Data.List (sort)
import Data.Time.Relative
  ( Format (..),
    FormatStyle (..),
    FormatVerbosity (..),
    RelativeTime (..),
  )
import Data.Time.Relative qualified as Relative
import GHC.Natural (Natural)
import Hedgehog (Gen, (===))
import Hedgehog qualified as H
import Hedgehog.Gen qualified as HG
import Hedgehog.Range qualified as HR
import Numeric.Algebra (AMonoid (..), ASemigroup (..))
import Test.Tasty (TestTree)
import Test.Tasty qualified as Tasty
import Test.Tasty.HUnit (testCase, (@=?))
import Test.Tasty.Hedgehog qualified as TastyH
import Text.Read qualified as TR

tests :: TestTree
tests =
  Tasty.testGroup
    "Unit"
    [ specs,
      props
    ]

specs :: TestTree
specs =
  Tasty.testGroup
    "Specs"
    [ formatProseCompact,
      formatProseFull,
      formatDigitalCompact,
      formatDigitalFull,
      testToString
    ]

formatProseCompact :: TestTree
formatProseCompact =
  Tasty.testGroup
    "Format Prose Compact"
    (mkFormatTests format expected)
  where
    expected =
      [ "0 seconds",
        "1 minute, 1 second",
        "3 minutes",
        "3 minutes, 20 seconds",
        "1 hour, 1 second",
        "1 hour, 6 minutes, 40 seconds",
        "1 day, 3 hours, 46 minutes, 40 seconds",
        "0 seconds"
      ]
    format =
      MkFormat
        { style = FormatStyleProse,
          verbosity = FormatVerbosityCompact
        }

formatProseFull :: TestTree
formatProseFull =
  Tasty.testGroup
    "Format Prose Full"
    (mkFormatTests format expected)
  where
    expected =
      [ "0 days, 0 hours, 0 minutes, 0 seconds",
        "0 days, 0 hours, 1 minute, 1 second",
        "0 days, 0 hours, 3 minutes, 0 seconds",
        "0 days, 0 hours, 3 minutes, 20 seconds",
        "0 days, 1 hour, 0 minutes, 1 second",
        "0 days, 1 hour, 6 minutes, 40 seconds",
        "1 day, 3 hours, 46 minutes, 40 seconds"
      ]
    format =
      MkFormat
        { style = FormatStyleProse,
          verbosity = FormatVerbosityFull
        }

formatDigitalCompact :: TestTree
formatDigitalCompact =
  Tasty.testGroup
    "Format Digital Compact"
    (mkFormatTests format expected)
  where
    expected =
      [ "00",
        "01:01",
        "03:00",
        "03:20",
        "01:00:01",
        "01:06:40",
        "01:03:46:40"
      ]
    format =
      MkFormat
        { style = FormatStyleDigital,
          verbosity = FormatVerbosityCompact
        }

formatDigitalFull :: TestTree
formatDigitalFull =
  Tasty.testGroup
    "Format Digital Full"
    (mkFormatTests format expected)
  where
    expected =
      [ "00:00:00:00",
        "00:00:01:01",
        "00:00:03:00",
        "00:00:03:20",
        "00:01:00:01",
        "00:01:06:40",
        "01:03:46:40"
      ]
    format =
      MkFormat
        { style = FormatStyleDigital,
          verbosity = FormatVerbosityFull
        }

testToString :: TestTree
testToString = testCase "toString" $ do
  "0" @=? Relative.toString (MkRelativeTime 0 0 0 0)
  "1d" @=? Relative.toString (MkRelativeTime 1 0 0 0)
  "2h" @=? Relative.toString (MkRelativeTime 0 2 0 0)
  "3m" @=? Relative.toString (MkRelativeTime 0 0 3 0)
  "4s" @=? Relative.toString (MkRelativeTime 0 0 0 4)
  "4d3h2m1s" @=? Relative.toString (MkRelativeTime 4 3 2 1)

mkFormatTests :: Format -> [String] -> [TestTree]
mkFormatTests f = zipWith (testFormat f) formatResults
  where
    testFormat :: Format -> Natural -> String -> TestTree
    testFormat fmt seconds' expectation =
      testCase (show seconds' ++ " should be " ++ expectation) $
        expectation @=? Relative.formatSeconds fmt seconds'

    formatResults =
      [ 0,
        61,
        180,
        200,
        3_601,
        4_000,
        100_000
      ]

props :: TestTree
props =
  Tasty.testGroup
    "Properties"
    [ testToFromId,
      testEq,
      testOrd,
      testNormalize,
      testDiff,
      testReadShowId,
      testRead,
      testFromString,
      testStringRoundTrip
    ]

testToFromId :: TestTree
testToFromId =
  TastyH.testPropertyNamed "toSeconds . fromSeconds = id" "testToFromId" $
    H.property $ do
      seconds' <- H.forAll gnat
      seconds' === f seconds'
  where
    f = Relative.toSeconds . Relative.fromSeconds

testEq :: TestTree
testEq =
  TastyH.testPropertyNamed "Eq is determined by seconds" "testEq" $
    H.property $ do
      r1 <- H.forAll grelativeTime
      r2 <- H.forAll grelativeTime
      let r1' = Relative.toSeconds r1
      let r2' = Relative.toSeconds r2
      H.annotateShow r1'
      H.annotateShow r2'
      (r1 == r2) === (r1' == r2')
      r1 === Relative.normalize r1

testOrd :: TestTree
testOrd =
  TastyH.testPropertyNamed "Ord is determined by seconds" "testOrd" $
    H.property $ do
      r1 <- H.forAll grelativeTime
      r2 <- H.forAll grelativeTime
      let r1' = Relative.toSeconds r1
      let r2' = Relative.toSeconds r2
      H.annotateShow r1'
      H.annotateShow r2'
      (r1 <= r2) === (r1' <= r2')

testNormalize :: TestTree
testNormalize =
  TastyH.testPropertyNamed "normalize rt reduces all fields" "testNormalize" $
    H.property $ do
      rt <- H.forAll grelativeTime
      let rt'@(MkRelativeTime _ h m s) = Relative.normalize rt
      H.annotateShow rt'
      H.assert $ h < 24
      H.assert $ m < 60
      H.assert $ s < 60

testDiff :: TestTree
testDiff =
  TastyH.testPropertyNamed "diffRelativeTime additive algebra" "testDiff" $
    H.property $ do
      r1 <- H.forAll grelativeTime
      r2 <- H.forAll grelativeTime
      let result = Relative.diffRelativeTime r1 r2
      H.annotateShow result
      if
        | r1 > r2 -> r1 === r2 .+. result
        | r2 > r1 -> r2 === r1 .+. result
        | otherwise -> zero === result

testReadShowId :: TestTree
testReadShowId =
  TastyH.testPropertyNamed "read . show = id" "testReadShowId" $
    H.property $ do
      rt <- H.forAll grelativeTime
      let rt' = f rt
      H.annotateShow rt'
      rt === rt'
  where
    f = read . show

testRead :: TestTree
testRead =
  TastyH.testPropertyNamed "Reads strings" "testRead" $
    H.property $ do
      s <- H.forAll greadable
      let result = TR.readEither @RelativeTime s
      case result of
        Left err -> do
          H.annotate $ "Failed to read str. Received: " <> err
          H.failure
        Right _ -> pure ()

testFromString :: TestTree
testFromString =
  TastyH.testPropertyNamed "fromString with Time String and Seconds succeeds" "testFromString" $
    H.property $ do
      s <- H.forAll gsecOrTimeStr
      let result = Relative.fromString s
      case result of
        Left err -> do
          H.annotate $ "Failed to parse str. Received: " <> err
          H.failure
        Right _ -> pure ()

testStringRoundTrip :: TestTree
testStringRoundTrip = TastyH.testPropertyNamed desc "testStringRoundTrip" $ do
  H.property $ do
    rt <- H.forAll grelativeTime
    let str = Relative.toString rt

    H.annotate str

    case Relative.fromString str of
      Left err -> H.annotate $ "Failed to parse str: " <> err
      Right result -> rt === result
  where
    desc = "fromString . toString round trips"

gnat :: Gen Natural
gnat = HG.integral $ HR.exponential 0 1_000_000

grelativeTime :: Gen RelativeTime
grelativeTime =
  MkRelativeTime
    <$> gnat
    <*> gnat
    <*> gnat
    <*> gnat

greadable :: Gen String
greadable = HG.choice [gnatStr, grelativeTimeStr, gtimeStr]

gsecOrTimeStr :: Gen String
gsecOrTimeStr = HG.choice [gnatStr, gtimeStr]

gnatStr :: Gen String
gnatStr = show <$> gnat

grelativeTimeStr :: Gen String
grelativeTimeStr = show <$> grelativeTime

gtimeStr :: Gen String
gtimeStr = do
  toTake <- HG.integral (HR.linear 1 4)
  fullStrShuffled <- gAllUnitsShuffled
  let someUnits = take toTake fullStrShuffled
  pure (unitToStr =<< sort someUnits)

data TimeStrUnit
  = Days Natural
  | Hours Natural
  | Minutes Natural
  | Seconds Natural
  deriving stock (Eq, Show)

instance Ord TimeStrUnit where
  Days _ `compare` Days _ = EQ
  Days _ `compare` _ = LT
  _ `compare` Days _ = GT
  Hours _ `compare` Hours _ = EQ
  Hours _ `compare` _ = LT
  _ `compare` Hours _ = GT
  Minutes _ `compare` Minutes _ = EQ
  Minutes _ `compare` _ = LT
  _ `compare` Minutes _ = GT
  Seconds _ `compare` Seconds _ = EQ

gAllUnitsShuffled :: Gen [TimeStrUnit]
gAllUnitsShuffled = do
  d <- gnat
  h <- gnat
  m <- gnat
  s <- gnat
  HG.shuffle [Days d, Hours h, Minutes m, Seconds s]

unitToStr :: TimeStrUnit -> String
unitToStr (Days d) = show d <> "d"
unitToStr (Hours h) = show h <> "h"
unitToStr (Minutes m) = show m <> "m"
unitToStr (Seconds s) = show s <> "s"
