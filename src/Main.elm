port module Main exposing (main)

import Browser exposing (Document)
import Browser.Events exposing (onAnimationFrameDelta)
import Css exposing (Style)
import Css.Animations as Anims
import Html.Styled as Html exposing (Html, toUnstyled)
import Html.Styled.Attributes as Attrs
import Html.Styled.Events as Events
import Json.Decode as Decode exposing (Decoder, Value)
import Json.Encode as Encode
import Process
import Random exposing (Generator)
import Task
import Time exposing (Month(..))


main : Program Value Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



---- TYPES ----


type alias Model =
    { slime : Maybe Slime
    , timeSinceInteraction : Float
    , currentHour : Int
    , events : List Event
    , today : ( Month, Int )
    }


type Msg
    = NoOp
    | Tick Float
    | SlimeClicked
    | ClickedEgg
    | ClickedFood
    | NewSlime Slime
    | SetToday ( Month, Int )
    | SaveSlime


type alias Event =
    { month : Month
    , day : Int
    , image : String
    }


type alias Slime =
    { state : State
    , color : SlimeColor
    , hunger : Float
    , timeSincePlayedWith : Float
    , birthday : Event
    }


type SlimeColor
    = Blue
    | Red
    | Green
    | Black
    | White


type State
    = Alive { mood : Mood, action : Action }
    | Dead


type Mood
    = Content
    | Shocked
    | Confused
    | Happy
    | Hungry
    | Upset


type Action
    = Sitting
    | Sleeping ( SleepState, Float )
    | Eating ( EatState, Float )
    | Jumping ( JumpState, Float )


type SleepState
    = Sleep1
    | Sleep2


type EatState
    = Eat1
    | Eat2
    | Eat3
    | Eat4


type JumpState
    = Jump1
    | Jump2
    | Jump3
    | Jump4
    | Jump5
    | Jump6



---- INIT ----


init : Value -> ( Model, Cmd Msg )
init slime =
    ( { slime = slimeDecoder slime
      , timeSinceInteraction = 0
      , currentHour = 0
      , events =
            [ { month = Dec, day = 2, image = "xmas" }
            , { month = Jan, day = 1, image = "newyear" }
            ]
      , today = ( Jan, 0 )
      }
    , getToday |> Task.perform SetToday
    )


slimeDecoder : Value -> Maybe Slime
slimeDecoder slime =
    case Decode.decodeValue decodeSlime slime of
        Ok s ->
            Just s

        Err _ ->
            Nothing


decodeSlime : Decoder Slime
decodeSlime =
    Decode.map5 Slime
        (Decode.field "state" decodeState)
        (Decode.field "color" decodeColor)
        (Decode.field "hunger" Decode.float)
        (Decode.field "timeSincePlayedWith" Decode.float)
        (Decode.field "birthday" decodeEvent)


decodeEvent : Decoder Event
decodeEvent =
    Decode.map3 Event
        (Decode.field "month" decodeMonth)
        (Decode.field "day" Decode.int)
        (Decode.field "image" Decode.string)


decodeMonth : Decoder Month
decodeMonth =
    Decode.string
        |> Decode.andThen
            (\str ->
                case str of
                    "Jan" ->
                        Decode.succeed Jan

                    "Feb" ->
                        Decode.succeed Feb

                    "Mar" ->
                        Decode.succeed Mar

                    "Apr" ->
                        Decode.succeed Apr

                    "May" ->
                        Decode.succeed May

                    "Jun" ->
                        Decode.succeed Jun

                    "Jul" ->
                        Decode.succeed Jul

                    "Aug" ->
                        Decode.succeed Aug

                    "Sep" ->
                        Decode.succeed Sep

                    "Oct" ->
                        Decode.succeed Oct

                    "Nov" ->
                        Decode.succeed Nov

                    "Dec" ->
                        Decode.succeed Dec

                    m ->
                        Decode.fail <| "Unkown month: " ++ m
            )


decodeColor : Decoder SlimeColor
decodeColor =
    Decode.string
        |> Decode.andThen
            (\str ->
                case str of
                    "Red" ->
                        Decode.succeed Red

                    "Green" ->
                        Decode.succeed Green

                    "Blue" ->
                        Decode.succeed Blue

                    "Black" ->
                        Decode.succeed Black

                    "White" ->
                        Decode.succeed White

                    c ->
                        Decode.fail <| "Unkown color: " ++ c
            )


decodeState : Decoder State
decodeState =
    Decode.oneOf
        [ Decode.null Dead
        , Decode.string
            |> Decode.andThen
                (\str ->
                    case str of
                        "Content" ->
                            Decode.succeed <| Alive { mood = Content, action = Sitting }

                        "Shocked" ->
                            Decode.succeed <| Alive { mood = Shocked, action = Sitting }

                        "Confused" ->
                            Decode.succeed <| Alive { mood = Confused, action = Sitting }

                        "Happy" ->
                            Decode.succeed <| Alive { mood = Happy, action = Sitting }

                        "Hungry" ->
                            Decode.succeed <| Alive { mood = Hungry, action = Sitting }

                        "Upset" ->
                            Decode.succeed <| Alive { mood = Upset, action = Sitting }

                        m ->
                            Decode.fail <| "Unkown mood: " ++ m
                )
        ]


getToday : Task.Task x ( Month, Int )
getToday =
    Task.map2
        (\zone t -> ( Time.toMonth zone t, Time.toDay zone t ))
        Time.here
        Time.now



---- SUBSCRIPTIONS ----


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ onAnimationFrameDelta Tick
        , Time.every (minuteToMilli 1) (\_ -> SaveSlime)
        ]


port save : Value -> Cmd msg


encodeSlime : Maybe Slime -> Value
encodeSlime slime =
    case slime of
        Nothing ->
            Encode.null

        Just { state, color, hunger, timeSincePlayedWith, birthday } ->
            Encode.object
                [ ( "state", encodeState state )
                , ( "color", encodeColor color )
                , ( "hunger", Encode.float hunger )
                , ( "timeSincePlayedWith", Encode.float timeSincePlayedWith )
                , ( "birthday", encodeEvent birthday )
                ]


encodeEvent : Event -> Value
encodeEvent { month, day, image } =
    Encode.object
        [ ( "month", encodeMonth month )
        , ( "day", Encode.int day )
        , ( "image", Encode.string image )
        ]


encodeMonth : Month -> Value
encodeMonth month =
    Encode.string <|
        case month of
            Jan ->
                "Jan"

            Feb ->
                "Feb"

            Mar ->
                "Mar"

            Apr ->
                "Apr"

            May ->
                "May"

            Jun ->
                "Jun"

            Jul ->
                "Jul"

            Aug ->
                "Aug"

            Sep ->
                "Sep"

            Oct ->
                "Oct"

            Nov ->
                "Nov"

            Dec ->
                "Dec"


encodeColor : SlimeColor -> Value
encodeColor color =
    Encode.string <|
        case color of
            Red ->
                "Red"

            Green ->
                "Green"

            Blue ->
                "Blue"

            Black ->
                "Black"

            White ->
                "White"


encodeState : State -> Value
encodeState state =
    case state of
        Dead ->
            Encode.null

        Alive { mood } ->
            encodeMood mood


encodeMood : Mood -> Value
encodeMood mood =
    Encode.string <|
        case mood of
            Content ->
                "Content"

            Shocked ->
                "Shocked"

            Confused ->
                "Confused"

            Happy ->
                "Happy"

            Hungry ->
                "Hungry"

            Upset ->
                "Upset"



---- UPDATE ----


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ timeSinceInteraction } as model) =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        SaveSlime ->
            ( model, model.slime |> encodeSlime |> save )

        Tick delta ->
            case model.slime of
                Nothing ->
                    ( model, Cmd.none )

                Just slime ->
                    ( { model
                        | slime = Just <| slimeTick slime delta timeSinceInteraction
                        , timeSinceInteraction = timeSinceInteraction + delta
                      }
                    , Cmd.none
                    )

        SlimeClicked ->
            case model.slime of
                Nothing ->
                    ( model, Cmd.none )

                Just slime ->
                    ( { model | slime = Just <| slimeClicked slime, timeSinceInteraction = 0 }
                    , Cmd.none
                    )

        ClickedEgg ->
            ( model, Random.generate NewSlime <| newSlime model.today )

        ClickedFood ->
            case model.slime of
                Nothing ->
                    ( model, Cmd.none )

                Just slime ->
                    ( { model | slime = Just <| slimeFed slime, timeSinceInteraction = 0 }, Cmd.none )

        NewSlime slime ->
            ( { model | slime = Just slime, timeSinceInteraction = 0 }
            , Task.perform (\_ -> SaveSlime) (Task.succeed ())
            )

        SetToday today ->
            ( { model | today = today }
            , Process.sleep (minuteToMilli 5)
                |> Task.andThen (\_ -> getToday)
                |> Task.perform SetToday
            )


slimeFed : Slime -> Slime
slimeFed slime =
    let
        nextHunger =
            slime.hunger - hourToMilli 6
    in
    { slime
        | hunger = nextHunger
        , state =
            case slime.state of
                Dead ->
                    Dead

                Alive { mood, action } ->
                    if nextHunger <= hourToMilli -24 then
                        Dead

                    else
                        Alive
                            { mood = mood
                            , action =
                                case action of
                                    Sitting ->
                                        Eating ( Eat1, 0 )

                                    Sleeping _ ->
                                        Eating ( Eat1, 0 )

                                    other ->
                                        other
                            }
    }


newSlime : ( Month, Int ) -> Generator Slime
newSlime ( month, day ) =
    Random.weighted
        ( 40, Blue )
        [ ( 40, Red )
        , ( 40, Green )
        , ( 1, Black )
        , ( 1, White )
        ]
        |> Random.andThen
            (\color ->
                Random.constant
                    { state = Alive { mood = Content, action = Sitting }
                    , color = color
                    , hunger = 0
                    , timeSincePlayedWith = 0
                    , birthday =
                        { month = month
                        , day = day
                        , image = "bday"
                        }
                    }
            )


slimeClicked : Slime -> Slime
slimeClicked slime =
    let
        nextState =
            case slime.state of
                Dead ->
                    Dead

                Alive { mood, action } ->
                    Alive
                        { mood = mood
                        , action =
                            case action of
                                Sitting ->
                                    Jumping ( Jump1, 0 )

                                Sleeping _ ->
                                    Jumping ( Jump1, 0 )

                                other ->
                                    other
                        }
    in
    { slime | state = nextState, timeSincePlayedWith = 0 }


slimeTick : Slime -> Float -> Float -> Slime
slimeTick slime delta timeSinceInteraction =
    let
        nextTimeSincePlayedWith =
            slime.timeSincePlayedWith + delta

        hungerTickRate =
            case slime.state of
                Dead ->
                    0

                Alive { action } ->
                    case action of
                        Sleeping _ ->
                            0.1

                        Jumping _ ->
                            2

                        Sitting ->
                            1

                        Eating _ ->
                            0

        nextHunger =
            slime.hunger + delta * hungerTickRate

        nextMood =
            if nextHunger >= hourToMilli 24 then
                Hungry

            else if nextHunger <= hourToMilli -12 then
                Upset

            else if nextHunger < hourToMilli 6 && nextTimeSincePlayedWith <= minuteToMilli 5 then
                Happy

            else
                Content

        nextState =
            case slime.state of
                Dead ->
                    Dead

                Alive { action } ->
                    if nextHunger >= hourToMilli 48 then
                        Dead

                    else
                        Alive
                            { mood = nextMood
                            , action =
                                case action of
                                    Sitting ->
                                        case nextMood of
                                            Upset ->
                                                Sitting

                                            Hungry ->
                                                Sitting

                                            _ ->
                                                if timeSinceInteraction >= (10 * 60000) then
                                                    -- 5 min since the last time the slime was played with
                                                    Sleeping ( Sleep1, 0 )

                                                else
                                                    Sitting

                                    Sleeping ( state, time ) ->
                                        case nextMood of
                                            Upset ->
                                                Sitting

                                            Hungry ->
                                                Sitting

                                            _ ->
                                                let
                                                    newTime =
                                                        time + delta
                                                in
                                                if newTime > sleepTickTime then
                                                    case state of
                                                        Sleep1 ->
                                                            Sleeping ( Sleep2, newTime - sleepTickTime )

                                                        Sleep2 ->
                                                            Sleeping ( Sleep1, newTime - sleepTickTime )

                                                else
                                                    Sleeping ( state, newTime )

                                    Eating ( state, time ) ->
                                        let
                                            newTime =
                                                time + delta
                                        in
                                        if newTime > eatTickTime then
                                            case state of
                                                Eat1 ->
                                                    Eating ( Eat2, newTime - eatTickTime )

                                                Eat2 ->
                                                    Eating ( Eat3, newTime - eatTickTime )

                                                Eat3 ->
                                                    Eating ( Eat4, newTime - eatTickTime )

                                                Eat4 ->
                                                    Sitting

                                        else
                                            Eating ( state, newTime )

                                    Jumping ( state, time ) ->
                                        let
                                            newTime =
                                                time + delta
                                        in
                                        if newTime > jumpTickTime then
                                            case state of
                                                Jump1 ->
                                                    Jumping ( Jump2, newTime - jumpTickTime )

                                                Jump2 ->
                                                    Jumping ( Jump3, newTime - jumpTickTime )

                                                Jump3 ->
                                                    Jumping ( Jump4, newTime - jumpTickTime )

                                                Jump4 ->
                                                    Jumping ( Jump5, newTime - jumpTickTime )

                                                Jump5 ->
                                                    Jumping ( Jump6, newTime - jumpTickTime )

                                                Jump6 ->
                                                    Sitting

                                        else
                                            Jumping ( state, newTime )
                            }
    in
    { slime | state = nextState, hunger = nextHunger, timeSincePlayedWith = nextTimeSincePlayedWith }


sleepTickTime : Float
sleepTickTime =
    1000


eatTickTime : Float
eatTickTime =
    250


jumpTickTime : Float
jumpTickTime =
    100


hourToMilli : Float -> Float
hourToMilli =
    (*) 3600000


minuteToMilli : Float -> Float
minuteToMilli =
    (*) 60000



---- VIEW ----


view : Model -> Document Msg
view model =
    { title = "Slime Buddy"
    , body =
        [ toUnstyled <| viewGame model ]
    }


viewGame : Model -> Html Msg
viewGame ({ events, today } as model) =
    let
        ( month, day ) =
            today

        todaysEvent =
            case model.slime of
                Nothing ->
                    ""

                Just { birthday } ->
                    if birthday.month == month && birthday.day == day then
                        birthday.image

                    else
                        events
                            |> List.filter (\event -> event.month == month && event.day == day)
                            |> List.head
                            |> Maybe.withDefault { month = Jan, day = -1, image = "" }
                            |> .image
    in
    Html.div
        [ Attrs.css
            [ Css.position Css.absolute
            , Css.top <| Css.px 0
            , Css.bottom <| Css.px 0
            , Css.left <| Css.px 0
            , Css.right <| Css.px 0
            , Css.backgroundColor <| Css.rgb 49 49 49
            ]
        ]
        [ Html.div
            [ Attrs.css
                [ Css.property "user-select" "none"
                , Css.width <| Css.px 128
                , Css.height <| Css.px 158
                , Css.position Css.absolute
                , Css.top <| Css.pct 50
                , Css.left <| Css.pct 50
                , Css.transform <| Css.translate2 (Css.pct -50) (Css.pct -50)
                ]
            ]
            [ viewBackground
            , viewBackgroundImage
            , viewBackgroundSpecial todaysEvent
            , case model.slime of
                Nothing ->
                    Html.text ""

                Just slime ->
                    viewSlime slime
            , viewSlimeTheme model todaysEvent
            , viewSlimeClickArea
            , viewSun
            , viewActions model.slime

            -- Dev text
            -- , Html.div
            --     [ Attrs.css
            --         [ Css.position Css.absolute
            --         , Css.top <| Css.px 90
            --         , Css.left <| Css.px 0
            --         ]
            --     ]
            --     [ Html.text <| toString ((Maybe.withDefault { state = Alive { mood = Content, action = Sitting }
            --             , color = Blue
            --             , hunger = 0
            --             , timeSincePlayedWith = 0
            --             , birthday =
            --                 { month = Jan
            --                 , day = 22
            --                 , image = "carl"
            --                 }
            --             } model.slime) |> .birthday) ]
            ]
        ]


viewActions : Maybe Slime -> Html Msg
viewActions slime =
    Html.div
        [ Attrs.css
            [ Css.position Css.absolute
            , Css.bottom <| Css.px 0
            , Css.left <| Css.px 0
            , Css.height <| Css.px 32
            , Css.width <| Css.px 128
            , Css.backgroundColor <| Css.rgb 97 61 47
            ]
        ]
        [ viewEggButton slime
        , viewFoodButton slime
        ]


viewFoodButton : Maybe Slime -> Html Msg
viewFoodButton slime =
    pictureButton
        { onClick =
            Maybe.andThen
                (\{ state } ->
                    case state of
                        Dead ->
                            Nothing

                        Alive { action } ->
                            case action of
                                Sitting ->
                                    Just ClickedFood

                                Sleeping _ ->
                                    Just ClickedFood

                                _ ->
                                    Nothing
                )
                slime
        , imageUri = "./assets/buttons/feed_large.png"
        , offset = 32
        , label = "Feed Slime"
        }


viewEggButton : Maybe Slime -> Html Msg
viewEggButton slime =
    pictureButton
        { onClick =
            case slime of
                Nothing ->
                    Just ClickedEgg

                Just { state } ->
                    case state of
                        Dead ->
                            Just ClickedEgg

                        Alive _ ->
                            Nothing
        , imageUri = "./assets/buttons/new_large.png"
        , offset = 0
        , label = "New Slime"
        }


pictureButton :
    { onClick : Maybe msg
    , imageUri : String
    , offset : Float
    , label : String
    }
    -> Html msg
pictureButton { onClick, imageUri, offset, label } =
    Html.button
        [ Attrs.css
            [ Css.position Css.absolute
            , Css.height <| Css.px 32
            , Css.width <| Css.px 32
            , Css.bottom <| Css.px 0
            , Css.right <| Css.px offset
            , Css.cursor <|
                case onClick of
                    Just _ ->
                        Css.pointer

                    Nothing ->
                        Css.notAllowed
            , Css.opacity <|
                Css.num <|
                    case onClick of
                        Just _ ->
                            1

                        Nothing ->
                            0.5
            , Css.borderStyle Css.none
            , Css.backgroundColor (Css.rgba 0 0 0 0)
            , Css.backgroundImage (Css.url imageUri)
            , Css.backgroundSize (Css.px 32)
            ]
        , Attrs.attribute "aria-label" label
        , case onClick of
            Nothing ->
                Attrs.disabled True

            Just handler ->
                Events.onClick handler
        ]
        []


viewSlimeClickArea : Html Msg
viewSlimeClickArea =
    Html.div
        [ Attrs.css
            [ Css.position Css.absolute
            , Css.height <| Css.px 22
            , Css.width <| Css.px 35
            , Css.bottom <| Css.px 35
            , Css.left <| Css.px 29
            , Css.cursor Css.pointer
            ]
        , Events.onClick SlimeClicked
        ]
        []


viewBackgroundSpecial : String -> Html Msg
viewBackgroundSpecial theme =
    Html.div
        [ Attrs.css
            [ Css.width <| Css.px 128
            , Css.height <| Css.px 128
            , Css.position Css.absolute
            , Css.bottom <| Css.px 32
            , Css.left <| Css.px 0
            , Css.backgroundSize <| Css.px 128
            , Css.backgroundRepeat Css.noRepeat
            , Css.backgroundImage <| Css.url ("./assets/ground/" ++ theme ++ ".png")
            ]
        ]
        []


viewBackground : Html Msg
viewBackground =
    Html.div
        [ Attrs.css
            [ Css.width <| Css.px 128
            , Css.height <| Css.px 128
            , Css.backgroundImage <|
                Css.linearGradient2
                    Css.toBottomRight
                    (Css.stop <| Css.rgba 255 255 255 0.85)
                    (Css.stop <| Css.rgba 65 251 255 0.85)
                    []
            ]
        , Attrs.style "-webkit-app-region" "drag"
        ]
        []


viewBackgroundImage : Html Msg
viewBackgroundImage =
    Html.div
        [ Attrs.css
            [ Css.width <| Css.px 128
            , Css.height <| Css.px 128
            , Css.marginTop <| Css.px -128
            , Css.backgroundSize <| Css.px 128
            , Css.backgroundRepeat Css.noRepeat
            , Css.backgroundImage <| Css.url "./assets/ground/default.png"
            ]
        ]
        []


viewSlime : Slime -> Html Msg
viewSlime slime =
    let
        slimeColor =
            case slime.color of
                Blue ->
                    "blue"

                Red ->
                    "red"

                Green ->
                    "green"

                Black ->
                    "black"

                White ->
                    "white"
    in
    Html.div
        [ Attrs.css
            [ Css.width <| Css.px 128
            , Css.height <| Css.px 128
            , Css.marginTop <| Css.px -128
            , Css.backgroundImage <| Css.url ("./assets/slime/colors/" ++ slimeColor ++ ".png")
            , Css.backgroundSize <| Css.px (128 * 16)
            , Css.animationName <|
                Anims.keyframes
                    [ ( 0, [ Anims.property "background-position" "-512px -256px" ] )
                    , ( 100, [ Anims.property "background-position" "-640px -256px" ] )
                    ]
            , case slime.state of
                Dead ->
                    viewSlimeDead

                Alive { mood, action } ->
                    case action of
                        Sitting ->
                            case mood of
                                Content ->
                                    viewSlimeContent

                                Shocked ->
                                    viewSlimeShocked

                                Confused ->
                                    viewSlimeConfused

                                Happy ->
                                    viewSlimeHappy

                                Hungry ->
                                    viewSlimeHungry

                                Upset ->
                                    viewSlimeUpset

                        Sleeping ( state, _ ) ->
                            case state of
                                Sleep1 ->
                                    viewSlimeSleep1

                                Sleep2 ->
                                    viewSlimeSleep2

                        Eating ( state, _ ) ->
                            case state of
                                Eat1 ->
                                    viewSlimeEat1

                                Eat2 ->
                                    viewSlimeEat2

                                Eat3 ->
                                    viewSlimeEat1

                                Eat4 ->
                                    viewSlimeEat2

                        Jumping ( state, _ ) ->
                            case state of
                                Jump1 ->
                                    viewSlimeJump1

                                Jump2 ->
                                    viewSlimeJump2

                                Jump3 ->
                                    viewSlimeJump3

                                Jump4 ->
                                    viewSlimeJump4

                                Jump5 ->
                                    viewSlimeJump5

                                Jump6 ->
                                    viewSlimeJump6
            ]
        ]
        []


viewSlimeContent : Style
viewSlimeContent =
    Css.backgroundPosition2 (Css.px 0) (Css.px 0)


viewSlimeShocked : Style
viewSlimeShocked =
    Css.backgroundPosition2 (Css.px 0) (Css.px -128)


viewSlimeConfused : Style
viewSlimeConfused =
    Css.backgroundPosition2 (Css.px -128) (Css.px -128)


viewSlimeHappy : Style
viewSlimeHappy =
    Css.backgroundPosition2 (Css.px -256) (Css.px -128)


viewSlimeDead : Style
viewSlimeDead =
    Css.backgroundPosition2 (Css.px -384) (Css.px -128)


viewSlimeHungry : Style
viewSlimeHungry =
    Css.backgroundPosition2 (Css.px -512) (Css.px -128)


viewSlimeUpset : Style
viewSlimeUpset =
    Css.backgroundPosition2 (Css.px -640) (Css.px -128)


viewSlimeEat1 : Style
viewSlimeEat1 =
    Css.backgroundPosition2 (Css.px 0) (Css.px -256)


viewSlimeEat2 : Style
viewSlimeEat2 =
    Css.backgroundPosition2 (Css.px -128) (Css.px -256)


viewSlimeSleep1 : Style
viewSlimeSleep1 =
    Css.backgroundPosition2 (Css.px -512) (Css.px -256)


viewSlimeSleep2 : Style
viewSlimeSleep2 =
    Css.backgroundPosition2 (Css.px -640) (Css.px -256)


viewSlimeJump1 : Style
viewSlimeJump1 =
    Css.backgroundPosition2 (Css.px 0) (Css.px -384)


viewSlimeJump2 : Style
viewSlimeJump2 =
    Css.backgroundPosition2 (Css.px -128) (Css.px -384)


viewSlimeJump3 : Style
viewSlimeJump3 =
    Css.backgroundPosition2 (Css.px -256) (Css.px -384)


viewSlimeJump4 : Style
viewSlimeJump4 =
    Css.backgroundPosition2 (Css.px -384) (Css.px -384)


viewSlimeJump5 : Style
viewSlimeJump5 =
    Css.backgroundPosition2 (Css.px -512) (Css.px -384)


viewSlimeJump6 : Style
viewSlimeJump6 =
    Css.backgroundPosition2 (Css.px -640) (Css.px -384)


viewSlimeTheme : Model -> String -> Html Msg
viewSlimeTheme model theme =
    Html.div
        [ Attrs.css
            [ Css.width <| Css.px 128
            , Css.height <| Css.px 128
            , Css.position Css.absolute
            , Css.bottom <| Css.px 32
            , Css.left <| Css.px 0
            , Css.backgroundImage <| Css.url ("./assets/slime/costumes/" ++ theme ++ ".png")
            , Css.backgroundSize <| Css.px (128 * 16)
            , Css.animationName <|
                Anims.keyframes
                    [ ( 0, [ Anims.property "background-position" "-512px -256px" ] )
                    , ( 100, [ Anims.property "background-position" "-640px -256px" ] )
                    ]
            , case model.slime of
                Nothing ->
                    Css.property "" ""

                Just slime ->
                    case slime.state of
                        Dead ->
                            viewSlimeDead

                        Alive { mood, action } ->
                            case action of
                                Sitting ->
                                    case mood of
                                        Content ->
                                            viewSlimeContent

                                        Shocked ->
                                            viewSlimeShocked

                                        Confused ->
                                            viewSlimeConfused

                                        Happy ->
                                            viewSlimeHappy

                                        Hungry ->
                                            viewSlimeHungry

                                        Upset ->
                                            viewSlimeUpset

                                Sleeping ( state, _ ) ->
                                    case state of
                                        Sleep1 ->
                                            viewSlimeSleep1

                                        Sleep2 ->
                                            viewSlimeSleep2

                                Eating ( state, _ ) ->
                                    case state of
                                        Eat1 ->
                                            viewSlimeEat1

                                        Eat2 ->
                                            viewSlimeEat2

                                        Eat3 ->
                                            viewSlimeEat1

                                        Eat4 ->
                                            viewSlimeEat2

                                Jumping ( state, _ ) ->
                                    case state of
                                        Jump1 ->
                                            viewSlimeJump1

                                        Jump2 ->
                                            viewSlimeJump2

                                        Jump3 ->
                                            viewSlimeJump3

                                        Jump4 ->
                                            viewSlimeJump4

                                        Jump5 ->
                                            viewSlimeJump5

                                        Jump6 ->
                                            viewSlimeJump6
            ]
        ]
        []


viewSun : Html Msg
viewSun =
    Html.div
        [ Attrs.css
            [ Css.backgroundColor <| Css.rgb 232 230 36
            , Css.borderBottomRightRadius <| Css.px 20
            , Css.width <| Css.px 20
            , Css.height <| Css.px 20
            , Css.position Css.absolute
            , Css.top <| Css.px 0
            , Css.left <| Css.px 0
            ]
        ]
        []
