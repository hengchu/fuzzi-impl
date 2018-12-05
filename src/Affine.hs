module Affine where

import Data.Comp
import Data.Comp.Derive
import SyntaxExt

-- we have a slightly funny definition of affine:
-- a command c is affine if
-- given {G1}   c {G2} and any k > 0
--       {k G1} c {k G2, (0, 0)} also holds
-- this is true for simple straight light programs + while loops
-- extensions are case specific
class AffineCheck f where
  affineCheck :: AlgM m f Bool

$(derive [liftSum] [''AffineCheck])

instance AffineCheck Expr where

instance AffineCheck Cmd where

instance AffineCheck CTCHint where
