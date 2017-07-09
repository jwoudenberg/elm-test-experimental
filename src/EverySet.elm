module EverySet exposing (..)

import EveryDict exposing (EveryDict)


type alias EverySet a =
    EveryDict a ()


empty : EverySet a
empty =
    EveryDict.empty


insert : a -> EverySet a -> EverySet a
insert elem set =
    EveryDict.insert elem () set


eq : EverySet a -> EverySet a -> Bool
eq =
    EveryDict.eq


union : EverySet a -> EverySet a -> EverySet a
union =
    EveryDict.union


diff : EverySet a -> EverySet a -> EverySet a
diff =
    EveryDict.diff


concatMap : (a -> EverySet b) -> EverySet a -> EverySet b
concatMap fn set =
    EveryDict.foldl (\x () ys -> union (fn x) ys) empty set


toList : EverySet a -> List a
toList =
    EveryDict.keys


singleton : a -> EverySet a
singleton x =
    EveryDict.singleton x ()


isEmpty : EverySet a -> Bool
isEmpty =
    EveryDict.isEmpty


member : a -> EverySet a -> Bool
member =
    EveryDict.member
