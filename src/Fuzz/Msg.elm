module Fuzz.Msg exposing (end, from, randomWalk, step)

{-|


## Fuzzing Msg

@docs randomWalk, from, step, end

-}

import EverySet exposing (EverySet)
import Fuzz exposing (Fuzzer)
import Lazy
import Lazy.List exposing (LazyList)
import Random.Pcg as Random exposing (Generator)


type alias StateMachine state msg =
    List (Step state msg)


type alias Step state msg =
    { from : state
    , probability : Float
    , transition : Maybe (Transition state msg)
    }


type alias Transition state msg =
    { to : state
    , by : msg
    }


{-| Create a fuzzer for lists of messages.

First define a type representing the different states your application can be in.
Then describe how the Msg's in your application can take you from one state to another.

We've now defined a finite-state machine, a graph we can walk have a fuzzer walk across randomly.
Each `List Msg` generated by the returned fuzzer represents one such walk.

Random walks always start in the state passed to the first `from` and end when hitting an `end`.
It is important that each state has a way of reaching an `end` entry, or the random walk might have no end!

    type StateName
        = Start
        | LoggingIn
        | Done

    {-| This is the actual Msg type of your application. It is included here for the purpose of this example but would ordinarily be imported from your non-test code.
    -}
    type Msg
        = OpenLogIn
        | Input String
        | Cancel
        | Submit
        | LogOut

    messages : Fuzzer (List Msg)
    messages =
        randomWalk
            [ from Start
                [ step 1 OpenLogIn LoggingIn
                ]
            , from LoggingIn
                [ step 0.8 (Input "tom") LoggingIn
                , step 0.1 Cancel Start
                , step 0.1 Submit Done
                ]
            , from Done
                [ step 0.1 LogOut Start
                , end 0.9
                ]
            ]

-}
randomWalk : List (StateMachine state msg) -> Fuzzer (List msg)
randomWalk machineParts =
    let
        machine =
            List.concat machineParts

        deadEnds_ =
            deadEnds machine |> EverySet.toList
    in
    -- TODO: calculate the expected length of the list and return an invalid fuzzer if it's too large.
    if not <| List.isEmpty deadEnds_ then
        Fuzz.invalid <| "The transitions you define allows randomWalk to enter some state(s) from which it can never reach an end. Please fix these state(s):\n\n    " ++ toString deadEnds_
    else
        case List.head machine of
            Nothing ->
                Fuzz.invalid "You must provide randomWalk with at least one transition."

            Just { from } ->
                let
                    generator : Generator (List (Transition state msg))
                    generator =
                        generate machine from
                in
                Fuzz.custom generator (shrink from (ends machine) [])
                    |> Fuzz.map (List.map .by)


{-| Shrink a list of transitions.

This shrinker simplifies a list of transitions by attempting to remove cycles from it.
A cycle is a chain of state transitions starting and ending in the same state.

Two things this shrinker is not doing, but perhaps could be doing in the future:

  - Attempt to stop a chain early on an end state.
  - Attempt to replace certain paths with shortcuts. For instance: (A -> B -> C -> D) to (A -> B -> D)

-}
shrink :
    state
    -> EverySet state
    -> List (Transition state msg)
    -> List (Transition state msg)
    -> LazyList (List (Transition state msg))
shrink state ends previous next =
    case next of
        [] ->
            Lazy.List.empty

        x :: rest ->
            Lazy.lazy
                (\_ ->
                    let
                        recurse : LazyList (List (Transition state msg))
                        recurse =
                            shrink x.to ends (previous ++ [ x ]) rest

                        endsHere : Maybe (List (Transition state msg))
                        endsHere =
                            if EverySet.member state ends then
                                Just previous
                            else
                                Nothing

                        removedCycle : Maybe (List (Transition state msg))
                        removedCycle =
                            dropUntilState state next
                                |> Maybe.map ((++) previous)

                        simpler : LazyList (List (Transition state msg))
                        simpler =
                            List.filterMap identity [ endsHere, removedCycle ]
                                |> Lazy.List.fromList
                    in
                    Lazy.List.append simpler recurse
                        |> Lazy.force
                )


dropUntilState : state -> List (Transition state msg) -> Maybe (List (Transition state msg))
dropUntilState state xs =
    case xs of
        [] ->
            Nothing

        x :: rest ->
            if x.to == state then
                Just rest
            else
                dropUntilState state rest


generate : StateMachine state msg -> state -> Generator (List (Transition state msg))
generate machine start =
    generateHelper machine start []
        |> Random.map List.reverse


generateHelper :
    StateMachine state msg
    -> state
    -> List (Transition state msg)
    -> Generator (List (Transition state msg))
generateHelper machine start transitions =
    let
        fromStart : StateMachine state msg
        fromStart =
            List.filter (.from >> (==) start) machine

        recurse : Maybe (Transition state msg) -> Generator (List (Transition state msg))
        recurse step =
            case step of
                Nothing ->
                    Random.constant transitions

                Just transition ->
                    generateHelper machine transition.to (transition :: transitions)
    in
    if List.isEmpty fromStart then
        -- We need to handle this case separately because Random.frequency will crash if passed an empty list.
        Random.constant transitions
    else
        fromStart
            |> List.map (\{ probability, transition } -> ( probability, Random.constant transition ))
            |> Random.frequency
            |> Random.andThen recurse


{-| Find the states that have to no way of reaching an end state.
-}
deadEnds : StateMachine state msg -> EverySet state
deadEnds machine =
    EverySet.diff (states machine) (terminate machine)


{-| Find all the states used in a state machine.
-}
states : StateMachine state msg -> EverySet state
states machine =
    let
        addStates : Step state msg -> EverySet state -> EverySet state
        addStates step states =
            Maybe.map (.to >> EverySet.singleton) step.transition
                |> Maybe.withDefault EverySet.empty
                |> EverySet.union states
    in
    List.foldl addStates EverySet.empty machine


{-| Find the states that have some way of reaching an end state.
-}
terminate : StateMachine state msg -> EverySet state
terminate machine =
    let
        endStates : EverySet state
        endStates =
            ends machine
    in
    EverySet.concatMap (\end -> anyBefore end machine) endStates


{-| Find states for which certain transitions exist.
-}
select : (Step state msg -> Bool) -> StateMachine state msg -> EverySet state
select predicate machine =
    let
        foldHelper : Step state msg -> EverySet state -> EverySet state
        foldHelper step states =
            if predicate step then
                EverySet.insert step.from states
            else
                states
    in
    List.foldl foldHelper EverySet.empty machine


{-| Find the states that have a transition towards some other state.
-}
oneBefore : state -> StateMachine state msg -> EverySet state
oneBefore target machine =
    let
        predicate : Step state msg -> Bool
        predicate step =
            case step.transition of
                Nothing ->
                    False

                Just { to } ->
                    to == target
    in
    select predicate machine


{-| Find the states that have a path of transitions towards some other state.
-}
anyBefore : state -> StateMachine state msg -> EverySet state
anyBefore target machine =
    anyBeforeHelper (EverySet.singleton target) machine


anyBeforeHelper : EverySet state -> StateMachine state msg -> EverySet state
anyBeforeHelper targets machine =
    let
        newMatches =
            EverySet.concatMap (\state -> oneBefore state machine) targets
                |> EverySet.union targets
    in
    if EverySet.eq targets newMatches then
        targets
    else
        anyBeforeHelper newMatches machine


{-| Find the states that are ends, meaning they contain an `end` clause.
-}
ends : StateMachine state msg -> EverySet state
ends machine =
    let
        predicate : Step state msg -> Bool
        predicate step =
            step.transition == Nothing
    in
    select predicate machine


type alias TransitionTo state msg =
    ( Float, Maybe (Transition state msg) )


{-| A group of transitions starting from a common state.
-}
from : state -> List (TransitionTo state msg) -> StateMachine state msg
from fromState transitionsTo =
    transitionsTo
        |> List.map (\( prob, transition ) -> Step fromState prob transition)


{-| A transition towards a new state.
A transition has a certain likelyhood of occuring, a Msg triggering it, and an end state.
-}
step : Float -> msg -> state -> TransitionTo state msg
step probability msg to =
    ( probability, Just (Transition to msg) )


{-| Like `step`, but instead of transitioning to a new state this ends the random walk.
-}
end : Float -> TransitionTo state msg
end probability =
    ( probability, Nothing )
