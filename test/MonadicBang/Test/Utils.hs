{-# LANGUAGE CPP #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
#if MIN_VERSION_ghc(9,6,0)
{-# LANGUAGE ScopedTypeVariables #-}
#endif
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ViewPatterns #-}

module MonadicBang.Test.Utils where

import Control.Monad
import Data.Foldable
import Data.Function

import Control.Monad.Trans.Writer.CPS

import GHC.Stack

import GHC
import GHC.Driver.Errors.Types
import GHC.Types.Error
import GHC.Types.SourceError
import GHC.Utils.Outputable hiding ((<>))

import MonadicBang.Test.Utils.RunGhcParser
import MonadicBang.Utils
import Data.Monoid

data FailType
  = forall a . Show a => IncorrectResult { expectedValue :: a, actualValue :: a }
  | forall a . Outputable a => Didn'tFail { expectedFails :: [PsMessage], actualValue :: a }
  | FailedIncorrectly { expectedFails :: [PsMessage], actualFails :: [GhcMessage] }

data Fail = MkFail { error :: FailType, callStack :: CallStack }

type TestType = WriterT (DList Fail) IO ()

type Test = HasCallStack => TestType

runSuite :: TestType -> IO [Fail]
runSuite test = fromDList <$> execWriterT test

prettyFail :: Fail -> SDoc
prettyFail failure = vcat
  [ case failure.error of
      IncorrectResult{ expectedValue, actualValue } -> vcat
        [ text "Expected: " <+> text (show expectedValue)
        , text "but got:  " <+> text (show actualValue)
        ]
      Didn'tFail{ expectedFails } -> vcat
        [ text "Expected failure with"
        , nest 2 $ diagnosticsSDoc expectedFails
        , text "but execution succeeded"
        ]
      FailedIncorrectly{ expectedFails, actualFails } -> vcat
        [ text "Expected failure with"
        , nest 2 $ diagnosticsSDoc expectedFails
        , text "but execution failed with these errors instead:"
        , nest 2 $ diagnosticsSDoc actualFails
        ]
  , text "at" <+> text (prettyCallStack failure.callStack)
  ]
  where
    diagnosticsSDoc diags = vcat (map (vcat . unDecorated . diagMsg) diags)

recordFail :: HasCallStack => FailType -> TestType
recordFail err = tell . Endo . (:) $ MkFail err callStack

assertEq :: (HasCallStack, Show a, Eq a) => a -> a -> TestType
assertEq expected actual = when (expected /= actual) $
  withFrozenCallStack $ recordFail $ IncorrectResult expected actual

sdocEq :: SDoc -> SDoc -> Bool
sdocEq = (==) `on` showSDocUnsafe

assertFailWith :: (HasCallStack, Outputable a) => [PsMessage] -> Either SourceError a -> TestType
assertFailWith expected = \case
  Right result -> withFrozenCallStack $ recordFail $ Didn'tFail expected result
  Left err -> unless sameFails do
    withFrozenCallStack $ recordFail $ FailedIncorrectly expected errMsgs
    where
      errMsgs = toList (srcErrorMessages err)
      toPsMessage = \case
        GhcPsMessage m -> Just m
        _ -> Nothing
      listEq eq xs ys = and $ zipWith eq xs ys
      sameFails = maybe False (((listEq . listEq) sdocEq `on` map (unDecorated . diagMsg)) expected) $ traverse toPsMessage errMsgs

diagMsg :: forall a . Diagnostic a => a -> DecoratedSDoc
#if MIN_VERSION_ghc(9,6,0)
diagMsg = diagnosticMessage (defaultDiagnosticOpts @a)
#else
diagMsg = diagnosticMessage
#endif

assertParseFailWith :: HasCallStack => [PsMessage] -> String -> TestType
assertParseFailWith expected source = withFrozenCallStack $
  assertFailWith expected . fmap pm_parsed_source =<< parseGhc source
