module While.Syntax  (
    Z,
    Var,
    Aexp(..),
    Bexp(..),
    Stm(..)
  ) where

type Z = Integer

type Var = String

-- | Arithmetic expressions
data Aexp = Const Z       -- ^ @n@
          | Var Var       -- ^ @x@
          | Add Aexp Aexp -- ^ @a1 + a2@
          | Sub Aexp Aexp -- ^ @a1 - a2@
          | Mul Aexp Aexp -- ^ @a1 * a2@
          | Div Aexp Aexp -- ^ @a1 / a2@
  deriving (Eq, Ord, Show, Read)

-- | Boolean expressions
data Bexp = BTrue         -- ^ @true@
          | BFalse        -- ^ @false@
          | Eq Aexp Aexp  -- ^ @a1 = a2@
          | Le Aexp Aexp  -- ^ @a1 <= a2@
          | Not Bexp      -- ^ @~ b@
          | And Bexp Bexp -- ^ @b1 && b2@
  deriving (Eq, Ord, Show, Read)

-- | Statements expressions
data Stm  = Assign Var Aexp -- ^ @x := a@
          | Skip            -- ^ @skip@
          | Seq Stm Stm     -- ^ @S1 ; S2@
          | If Bexp Stm Stm -- ^ @if b then S1 else S2@
          | While Bexp Stm  -- ^ @while b S@
          | Or Stm Stm      -- ^ @or/par S1 S2@
  deriving (Eq, Ord, Show, Read)
