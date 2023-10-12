{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE CPP #-}

module MonadicBang.Error where

import Prelude hiding ((<>))

import Control.Effect.Writer

import GHC
#if MIN_VERSION_ghc(9,8,0)
import GHC.Utils.Error
#endif
import GHC.Types.Error
import GHC.Types.Name.Occurrence
import GHC.Parser.Errors.Types
import GHC.Utils.Outputable

data Error = ErrOutOfScopeVariable OccName
           | ErrBangOutsideOfDo

type PsErrors = Writer (Messages PsError)

customError :: Error -> PsError
#if MIN_VERSION_ghc(9,8,0)
customError = PsUnknownMessage . mkUnknownDiagnostic . \cases
#elif MIN_VERSION_ghc(9,6,0)
customError = PsUnknownMessage . UnknownDiagnostic . \cases
#else
customError = PsUnknownMessage . \cases
#endif
  ErrBangOutsideOfDo -> DiagnosticMessage
    { diagMessage = mkDecorated [text "Monadic ! outside of a 'do'-block is not allowed"]
    , diagReason = ErrorWithoutFlag
    , diagHints = [SuggestMissingDo]
    }
  (ErrOutOfScopeVariable name) -> DiagnosticMessage
    { diagMessage = mkDecorated [text "The variable " <> quotes (ppr name) <> text " cannot be used inside of ! here, since its desugaring would escape its scope"]
    , diagReason = ErrorWithoutFlag
    , diagHints = [UnknownHint $ text "Maybe you meant to open a new 'do'-block after " <> ppr name <> text " has been bound?"]
    }

tellPsError :: Has PsErrors sig m => PsError -> SrcSpan -> m ()
tellPsError err srcSpan = tell . singleMessage $
#if MIN_VERSION_ghc(9,8,0)
  mkErrorMsgEnvelope srcSpan neverQualify err
#else
  MsgEnvelope srcSpan neverQualify err SevError
#endif
