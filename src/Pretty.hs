{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}

module Pretty where

import Data.Map
import Data.Map as M

import Syntax
import Prelude hiding (LT, EQ, GT)
import Text.PrettyPrint

precedence :: Map Binop Int
precedence = M.fromList [(OR, 0), (AND, 1),
                         (EQ, 2), (NEQ, 2),
                         (LT, 3), (LE, 3), (GT, 3), (GE, 3),
                         (PLUS, 4), (MINUS, 4),
                         (MULT, 5), (DIV, 5)
                        ]

fixity :: Map Binop Int
fixity = M.fromList [(OR, 1), (AND, 1),
                     (EQ, 0), (NEQ, 0),
                     (LT, 0), (LE, 0), (GT, 0), (GE, 0),
                     (PLUS, 1), (MINUS, 1),
                     (MULT, 1), (DIV, 1)
                    ]

opDoc :: Map Binop Doc
opDoc = M.fromList [(OR, text "||"), (AND, text "&&"),
                    (EQ, equals), (NEQ, text "!="),
                    (LT, text "<"), (LE, text "<="),
                    (GT, text ">"), (GE, text ">="),
                    (PLUS, text "+"), (MINUS, text "-"),
                    (MULT, text "*"), (DIV, text "/")
                   ]

prettyExpr :: Expr -> Int -> Doc
prettyExpr e =
  foldExpr prettyVar prettyLen prettyLit prettyBinop
           prettyIndex prettyUpdate prettyAccess prettyClip
           e

  where prettyVar _ x = \_ -> text x

        prettyLen _ pe = \_ -> text "length" <> lparen <> pe 0 <> rparen

        prettySmallLit = \case
          SILit i -> int i
          SFLit f -> float f
          SBLit b -> text $ if b then "true" else "false"

        prettyRowLit rlit =
          M.foldrWithKey
            (\label slit doc -> text label <+> equals <+> prettySmallLit slit <> comma <+> doc)
            mempty
            rlit

        prettyLit _ lit = \_ ->
          case lit of
            SLit slit -> prettySmallLit slit
            RLit (getRowLits -> rlit) ->
              lbrace <> prettyRowLit rlit <> rbrace

        parensIf cond doc = if cond then lparen <> doc <> rparen else doc

        prettyBinop _ plhs op prhs = \prec ->
          let f = fixity     ! op
              p = precedence ! op
          in parensIf (prec > p) $ (plhs p) <+> opDoc ! op <+> (prhs (p + f))

        prettyIndex _ parr pidx = \prec ->
          parr prec <> lbrack <> pidx 0 <> rbrack

        prettyUpdate _ prow label pvalue = \prec ->
          prow prec <> lbrace <+> text label <+> equals <+> pvalue 0 <+> rbrace

        prettyAccess _ prow label = \prec ->
          prow prec <> text "." <> text label

        prettyClip posn pv lit = \_ ->
          text "clip" <> lparen <> pv 0 <> comma <+> prettyLit posn lit (0 :: Int) <> rparen

prettyCmd :: Cmd -> Doc
prettyCmd =
  foldCmd prettyAssign prettyAupdate prettyLupdate
          prettyLaplace prettyIf prettyWhile prettyDecl
          prettySeq prettySkip prettyBmap prettyAmap
          prettyBsum prettyPartition
  where
    prettyAssign _ x e = text x <+> equals <+> prettyExpr e 0

    prettyAupdate _ earr eidx erhs =
      prettyExpr earr 0 <> lbrack <> prettyExpr eidx 0 <> rbrack <+> equals <+> prettyExpr erhs 0

    prettyLupdate _ earr erhs =
      text "length" <> lparen <> prettyExpr earr 0 <> rparen <+> equals <+> prettyExpr erhs 0

    prettyLaplace _ x width e =
      text x <+> text "$=" <+> text "laplace" <> lparen <> float width <> comma <+> prettyExpr e 0 <> rparen

    prettyIf _ e ct cf =
      text "if" <+> prettyExpr e 0 <+> text "then"
      $$
      nest 2 ct
      $$
      text "else"
      $$
      nest 2 cf
      $$
      text "end"

    prettyWhile _ e c =
      text "while" <+> prettyExpr e 0 <+> text "do"
      $$
      nest 2 c
      $$
      text "end"

    prettyST = \case
      STInt -> text "int"
      STFloat -> text "float"
      STBool -> text "bool"

    prettyRT rt =
      M.foldrWithKey (\label st doc -> text label <+> colon <+> prettyST st <> comma <+> doc) mempty rt

    prettyLT = \case
      LTSmall st -> prettyST st
      LTRow (getRowTypes -> rt) -> lbrace <+> prettyRT rt <+> rbrace
      LTArray st -> lbrack <> prettyST st <> rbrack
      LTBag lt -> lbrace <> prettyLT lt <> rbrace

    prettyDecl _ x s t =
      text x <+> colon <> lbrack <> float s <> rbrack <+> prettyLT t

    prettySeq _ c1 c2 =
      c1 <> text ";"
      $$
      c2

    prettySkip _ = text "skip"

    prettyBmap _ invar outvar tvar ivar outtemp c =
      text "bmap"
        <> lparen
        <> text invar
        <> comma <+> text outvar
        <> comma <+> text tvar
        <> comma <+> text ivar
        <> comma <+> text outtemp
        <> comma <+> lbrace
        $$ nest 2 c
        $$ rbrace
        <> rparen

    prettyAmap _ invar outvar tvar ivar outtemp c =
      text "amap"
        <> lparen
        <> text invar
        <> comma <+> text outvar
        <> comma <+> text tvar
        <> comma <+> text ivar
        <> comma <+> text outtemp
        <> comma <+> lbrace
        $$ nest 2 c
        $$ rbrace
        <> rparen

    prettyBsum posn invar outvar tvar ivar b =
      text "bsum"
        <> lparen
        <> text invar
        <> comma <+> text outvar
        <> comma <+> text tvar
        <> comma <+> text ivar
        <> comma <+> prettyExpr (ELit posn b) 0
        <> rparen

    prettyPartition _ invar outvar tvar ivar outindex c =
      text "partition"
        <> lparen
        <> text invar
        <> comma <+> text outvar
        <> comma <+> text tvar
        <> comma <+> text ivar
        <> comma <+> text outindex
        <> comma <+> lbrace
        $$
        nest 2 c
        $$ rbrace
        <> rparen
