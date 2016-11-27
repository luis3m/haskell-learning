data TrafficLight = Red | Yellow | Green

instance Eq TrafficLight where
  Red == Red = True
  Yellow == Yellow = True
  Green == Green = True
  _ == _ = False

class Boolish a where
  truthy :: a -> Bool

instance Boolish Int where
  truthy 0 = False
  truthy _ = True

instance Boolish [a] where
  truthy [] = False
  truthy _  = True

instance Boolish Bool where
  truthy = id

instance Boolish TrafficLight where
  truthy Red = False
  truthy _   = True

truthyIf :: (Boolish b) => b -> a -> a -> a
truthyIf boolish wtrue wfalse = if truthy boolish then wtrue else wfalse
