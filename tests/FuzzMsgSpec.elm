module FuzzMsgSpec exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)
import Fuzz.Msg exposing (..)
import Lazy.List
import Random.Pcg as Random
import Test exposing (..)
import Test.Runner


suite : Test
suite =
    describe "StateMachine"
        [ fuzz messages "Only genererates valid message lists" <|
            \fuzzedMessages ->
                valid Start fuzzedMessages
        , fuzz seed "Only shrinks into valid message lists" <|
            \fuzzedSeed ->
                let
                    ( value, shrinkable ) =
                        Test.Runner.fuzz messages
                            |> (\gen -> Random.step gen fuzzedSeed)
                            |> Tuple.first

                    shrink : List (List Msg) -> Test.Runner.Shrinkable (List Msg) -> List (List Msg)
                    shrink shrunken shrinkable =
                        case Test.Runner.shrink False shrinkable of
                            Nothing ->
                                shrunken

                            Just ( newShrunken, newShrinkable ) ->
                                shrink (newShrunken :: shrunken) newShrinkable
                in
                assertForAll (valid Start) (shrink [] shrinkable)
        , fuzz seed "Shrinks into short message lists" <|
            \fuzzedSeed ->
                shrinkMaximally fuzzedSeed messages
                    |> List.length
                    |> Expect.atMost 2
        , fuzz seed "Shrinker uses early end states when present" <|
            \fuzzedSeed ->
                shrinkMaximally fuzzedSeed earlyEndMessages
                    |> List.length
                    |> Expect.atMost 0
        ]


shrinkMaximally : Random.Seed -> Fuzzer (List Msg) -> List Msg
shrinkMaximally seed fuzzer =
    let
        ( value, shrinkable ) =
            Test.Runner.fuzz fuzzer
                |> (\gen -> Random.step gen seed)
                |> Tuple.first

        shrink : List Msg -> Test.Runner.Shrinkable (List Msg) -> List Msg
        shrink smallest shrinkable =
            case Test.Runner.shrink False shrinkable of
                Nothing ->
                    smallest

                Just ( newShrunken, newShrinkable ) ->
                    shrink newShrunken newShrinkable
    in
    shrink value shrinkable


{-| Whereas `Expect.all` allows you to run multiple assertions against a single piece of data,
`assertForall` allows you to run a single assertion against multiple pieces of data.
-}
assertForAll : (a -> Expect.Expectation) -> List a -> Expect.Expectation
assertForAll testFn cases =
    case cases of
        -- Expect.all will fail an empty list of expectations, which is not what we want here.
        [] ->
            Expect.pass

        nonEmptyCases ->
            nonEmptyCases
                |> List.map (\singleCase _ -> testFn singleCase)
                |> flip Expect.all ()


valid : StateName -> List Msg -> Expectation
valid startState messages =
    case ( startState, messages ) of
        ( Start, OpenLogIn :: xs ) ->
            valid LoggingIn xs

        ( LoggingIn, (Input "tom") :: xs ) ->
            valid LoggingIn xs

        ( LoggingIn, Cancel :: xs ) ->
            valid Start xs

        ( LoggingIn, Submit :: xs ) ->
            valid Done xs

        ( Done, LogOut :: xs ) ->
            valid Start xs

        ( Done, [] ) ->
            Expect.pass

        ( state, [] ) ->
            Expect.fail <| "Invalid series of messages, " ++ toString state ++ " is not an end state."

        ( state, x :: xs ) ->
            Expect.fail <|
                "Invalid series of messages, "
                    ++ toString x
                    ++ " is not a valid message in state "
                    ++ toString state
                    ++ "."


seed : Fuzzer Random.Seed
seed =
    Fuzz.custom Random.independentSeed (\_ -> Lazy.List.empty)


type StateName
    = Start
    | LoggingIn
    | Done


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


earlyEndMessages : Fuzzer (List Msg)
earlyEndMessages =
    randomWalk
        [ from Start
            [ step 1 OpenLogIn LoggingIn
            , end 0.1
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
