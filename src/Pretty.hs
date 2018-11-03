{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}

module Pretty (
  prettyCmd
  , prettyExpr
  , Pretty.render
  , PrettyCmd(..)
  , PrettyExpr(..)
  ) where

import Data.Map hiding (foldr)
import qualified Data.Map as M

import Syntax
import Prelude hiding (LT, EQ, GT, (<>))
import Text.PrettyPrint
import Test.QuickCheck

render :: Doc -> String
render = Text.PrettyPrint.render

precedence :: Map Binop Int
precedence = M.fromList [(OR, 0), (AND, 1),
                         (EQ, 2), (NEQ, 2),
                         (LT, 3), (LE, 3), (GT, 3), (GE, 3),
                         (PLUS, 4), (MINUS, 4),
                         (MULT, 5), (DIV, 5)
                        ]

getPrecedence :: Binop -> Int
getPrecedence op =
  case M.lookup op precedence of
    Nothing -> error $ "Undefined precedence for operator: " ++ show op
    Just p -> p

fixity :: Map Binop Int
fixity = M.fromList [(OR, 1), (AND, 1),
                     (EQ, 0), (NEQ, 0),
                     (LT, 0), (LE, 0), (GT, 0), (GE, 0),
                     (PLUS, 1), (MINUS, 1),
                     (MULT, 1), (DIV, 1)
                    ]

getFixity :: Binop -> Int
getFixity op =
  case M.lookup op fixity of
    Nothing -> error $ "Undefined fixity for operator: " ++ show op
    Just f -> f

opDoc :: Map Binop Doc
opDoc = M.fromList [(OR, text "||"), (AND, text "&&"),
                    (EQ, text "=="), (NEQ, text "!="),
                    (LT, text "<"), (LE, text "<="),
                    (GT, text ">"), (GE, text ">="),
                    (PLUS, text "+"), (MINUS, text "-"),
                    (MULT, text "*"), (DIV, text "/")
                   ]

parensIf :: Bool -> Doc -> Doc
parensIf cond doc = if cond then lparen <> doc <> rparen else doc

concatWith :: Doc -> [Doc] -> Doc
concatWith _ [] = mempty
concatWith _ (x:[]) = x
concatWith sep (x:xs) = x <> sep <> concatWith sep xs

prettyLit :: Lit -> Doc
prettyLit (LInt i) = int i
prettyLit (LFloat f) = float f
prettyLit (LBool b) = if b then text "true" else text "false"
prettyLit (LArr es) =
  brackets $ concatWith (comma <> text " ") $ fmap (flip prettyExpr 0) es
prettyLit (LBag es) =
  braces $ concatWith (comma <> text " ") $ fmap (flip prettyExpr 0) es

prettyExpr :: Expr -> Int -> Doc
prettyExpr (EVar _ x)    _ = text x
prettyExpr (ELength _ e) _ = text "length" <> (parens $ prettyExpr e 0)
prettyExpr (ELit _ lit)  _ = prettyLit lit
prettyExpr (EBinop _ e1 op e2) p =
  let opPrec = getPrecedence op
      opFixity = getFixity op
  in parensIf (p > opPrec)
       $ (prettyExpr e1 opPrec) <+> opDoc ! op <+> (prettyExpr e2 (opPrec + opFixity))
prettyExpr (EIndex _ e1 e2) _ =
  (parens $ prettyExpr e1 0) <> lbrack <> prettyExpr e2 0 <> rbrack
prettyExpr (ERAccess _ e label) _ =
  (parens $ prettyExpr e 0) <> text "." <> text label
prettyExpr (EFloat _ e) _ =
  text "fc" <> (parens $ prettyExpr e 0)
prettyExpr (EExp _ e) _ =
  text "exp" <> (parens $ prettyExpr e 0)
prettyExpr (ELog _ e) _ =
  text "log" <> (parens $ prettyExpr e 0)
prettyExpr (EClip _ e lit) _ =
  text "clip" <> (parens $ prettyExpr e 0 <> comma <+> prettyLit lit)
prettyExpr (EScale _ e1 e2) _ =
  text "scale" <> (parens $ prettyExpr e1 0 <> comma <+> prettyExpr e2 0)
prettyExpr (EDot _ e1 e2) _ =
  text "dot" <> (parens $ prettyExpr e1 0 <> comma <+> prettyExpr e2 0)

prettyParam :: Param -> Doc
prettyParam (PExpr e) = prettyExpr e 0
prettyParam (PCmd c) =
  vcat [
  lbrace
  , nest 2 $ prettyCmd c
  , rbrace
  ]

prettyParams :: [Param] -> Doc
prettyParams params =
  concatWith (comma <> text " ") $ fmap prettyParam params

prettyCmd :: Cmd -> Doc
prettyCmd (CAssign _ lhs rhs) =
  prettyExpr lhs 0 <+> equals <+> prettyExpr rhs 0
prettyCmd (CLaplace _ x b rhs) =
  text x <+> text "$=" <+> text "lap" <> (parens $ float b <> comma <+> prettyExpr rhs 0)
prettyCmd (CIf _ e c1 c2) =
  vcat [
  text "if" <+> prettyExpr e 0 <+> text "then"
  , nest 2 $ prettyCmd c1 <> text ";"
  , text "else"
  , nest 2 $ prettyCmd c2 <> text ";"
  , text "end"
  ]
prettyCmd (CWhile _ e c) =
  vcat [
  text "while" <+> prettyExpr e 0 <+> text "do"
  , nest 2 $ prettyCmd c <> text ";"
  , text "end"
  ]
prettyCmd (CSeq _ c1 c2) =
  vcat [
  prettyCmd c1 <> text ";"
  , prettyCmd c2
  ]
prettyCmd (CSkip _) =
  text "skip"
prettyCmd (CExt _ name params) =
  text name <> (parens $ prettyParams params)

newtype PrettyCmd = PrettyCmd Cmd
  deriving (Eq)
newtype PrettyExpr = PrettyExpr Expr
  deriving (Eq)

instance Show PrettyCmd where
  show (PrettyCmd c) = Pretty.render $ prettyCmd c

instance Show PrettyExpr where
  show (PrettyExpr e) = Pretty.render $ prettyExpr e 0

instance Arbitrary PrettyCmd where
  arbitrary = PrettyCmd <$> arbitrary
  shrink (PrettyCmd c) = PrettyCmd <$> shrink c

instance Arbitrary PrettyExpr where
  arbitrary = PrettyExpr <$> arbitrary
  shrink (PrettyExpr e) = PrettyExpr <$> shrink e
