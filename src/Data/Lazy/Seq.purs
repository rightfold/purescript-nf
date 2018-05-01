-- | Force the lazy thunk, returning the second argument.
module Data.Lazy.Seq
  ( seq
  ) where

import Data.Lazy (Lazy, force)

-- | Force the lazy thunk, returning the second argument.
seq :: ∀ a b. Lazy a -> b -> b
seq = seqFFI force

-- | Foreign so that a hypothetical optimizer would not eliminate it.
foreign import seqFFI :: ∀ a b. (∀ x. Lazy x -> x) -> Lazy a -> b -> b
