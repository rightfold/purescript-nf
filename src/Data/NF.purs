-- | Evaluate an expression to normal form.
-- |
-- | Example use cases include benchmarking operations on data structures with
-- | lazy parts, and unit testing programs that throw exceptions from within
-- | lazy thunks.
module Data.NF
  ( class NF
  , nf
  , deepseq

  , ($!!), applyNF
  , (#!!), applyNFFlipped
  ) where

import Prelude

import Data.Lazy (Lazy, force)
import Data.Lazy.Seq (seq)

import Data.List as List
import Data.List.Lazy as LList



-- | Evaluate an expression to normal form.
-- |
-- | Instances must satisfy the following laws:
-- |
-- |  - Normal form: `nf a` is in normal form.
-- |  - Idempotence: `nf (nf a) = nf a`.
class NF a where
  nf :: a -> a

-- | Force the expression, returning the second argument.
deepseq :: ∀ a b. NF a => a -> b -> b
deepseq = deepseqFFI nf

-- | Foreign so that a hypothetical optimizer would not eliminate it.
foreign import deepseqFFI :: ∀ a b. (a -> a) -> a -> b -> b

instance nfVoid :: NF Void where
  nf = absurd

instance nfUnit :: NF Unit where
  nf = id

instance nfBoolean :: NF Boolean where
  nf = id

instance nfInt :: NF Int where
  nf = id

instance nfNumber :: NF Number where
  nf = id

instance nfChar :: NF Char where
  nf = id

instance nfString :: NF String where
  nf = id

instance nfArray :: NF a => NF (Array a) where
  nf = map nf

instance nfList :: NF a => NF (List.List a) where
  nf = map nf

instance nfListLazy :: NF a => NF (LList.List a) where
  nf l = go l
    where
      go :: LList.List a -> LList.List a
      go (LList.List s) =
        case force s of
          LList.Nil -> l
          LList.Cons a as -> go (a `deepseq` as)

instance nfLazy :: NF a => NF (Lazy a) where
  nf x = x `seq` x



infixr 0 applyNF as $!!
infixr 1 applyNFFlipped as #!!

applyNF :: ∀ a b. NF a => (a -> b) -> a -> b
applyNF f x = f (nf x)

applyNFFlipped :: ∀ a b. NF a => a -> (a -> b) -> b
applyNFFlipped x f = f (nf x)
