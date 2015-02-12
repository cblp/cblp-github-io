module Local.Prelude where


(&) :: a -> (a -> b) -> b
x & f = f x
{-# INLINE (&) #-}
