import qualified Data.Map as Map

data LockerState = Taken | Free deriving (Show, Eq)
type Code = String
type LockerMap = Map.Map Int (LockerState, Code)

lockerLookup :: Int -> LockerMap -> Either String Code
lockerLookup number map =
  case Map.lookup number map of
    Nothing            -> Left $ "Locker number " ++ show number ++ " doesn't exist"
    Just (Taken, _   ) -> Left $ "Locker number " ++ show number ++ " is taken"
    Just (_    , code) -> Right code
