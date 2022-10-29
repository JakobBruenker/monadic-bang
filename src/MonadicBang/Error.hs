{-# LANGUAGE LambdaCase #-}

module MonadicBang.Error where

import Prelude hiding ((<>))

import Control.Effect.Writer

import GHC
import GHC.Types.Error
import GHC.Types.Name.Occurrence
import GHC.Parser.Errors.Types
import GHC.Utils.Outputable

data Error = ErrOutOfScopeVariable OccName
           | ErrBangOutsideOfDo

type PsErrors = Writer (Messages PsError)

customError :: Error -> PsError
customError = PsUnknownMessage . \cases
  ErrBangOutsideOfDo -> DiagnosticMessage
    { diagMessage = mkDecorated [text "Monadic ! outside of a 'do'-block is not allowed"]
    , diagReason = ErrorWithoutFlag
    , diagHints = [SuggestMissingDo]
    }
  (ErrOutOfScopeVariable name) -> DiagnosticMessage
    { diagMessage = mkDecorated [text "The variable " <> ppr name <> text " cannot be used inside of ! here, since its desugaring would escape its scope"]
    , diagReason = ErrorWithoutFlag
    , diagHints = [UnknownHint $ text "Maybe you meant to open a new 'do'-block after " <> ppr name <> text " has been bound?"]
    }

tellPsError :: Has PsErrors sig m => PsError -> SrcSpan -> m ()
tellPsError err srcSpan = tell . singleMessage $ MsgEnvelope srcSpan neverQualify err SevError
