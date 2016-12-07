{-# LANGUAGE ExistentialQuantification #-}

infixr 5 :<

data HList = forall s. Show s => s :< HList | HNil

infixr 5 .++

(.++) :: HList -> HList -> HList
HNil .++ xs      = xs
xs .++ HNil      = xs
(x :< xs) .++ (ys@(y :< _)) = x :< (xs .++ ys)

instance Show HList where
  show HNil      = "[]"
  show (s :< xs) = show s ++ " <: " ++ show xs
