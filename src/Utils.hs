module Utils (
  evalT,
  evalWithArgsT,
  compileD,
) where

import Data.Bifunctor (
  first,
 )
import Data.Text (
  Text,
  pack,
 )
import Plutarch (
  Config (Config),
  TracingMode (DoTracing),
  compile,
 )
import Plutarch.Evaluate (
  evalScript,
 )
import "liqwid-plutarch-extra" Plutarch.Extra.Script (
  applyArguments,
 )
import Plutarch.Prelude

import PlutusLedgerApi.V2 (
  Data,
  ExBudget,
 )

import Plutarch.Script (Script)

evalT :: ClosedTerm a -> Either Text (Script, ExBudget, [Text])
evalT x = evalWithArgsT x []

evalWithArgsT :: ClosedTerm a -> [Data] -> Either Text (Script, ExBudget, [Text])
evalWithArgsT x args = do
  cmp <- compile (Config DoTracing) x
  let (escr, budg, trc) = evalScript $ applyArguments cmp args
  scr <- first (pack . show) escr
  pure (scr, budg, trc)

compileD :: ClosedTerm a -> Script
compileD x =
  case evalT x of
    Left e -> error (show e)
    Right (a, _, _) -> a

