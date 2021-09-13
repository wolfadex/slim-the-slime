module Main exposing (main)

import Animator
import Browser
import Browser.Dom
import Browser.Events
import Canvas exposing (Renderable)
import Canvas.Settings
import Canvas.Settings.Text
import Canvas.Texture exposing (Texture)
import Color
import Json.Decode as Decode
import Task
import Time



{--}
type alias Model =
    { x : Float
    , y : Float
    , vx : Float
    , vy : Float
    , isGrounded : Bool
    , window : Window
    , gamepad : GamePad
    , slime : Animator.Timeline Slime
    , sprites : Loadable Sprites
    }


type Loadable a
    = Loading
    | Loaded a
    | Failure


type alias Sprites =
    { player :
        { right :
            { stand : Texture
            , walk : Texture
            , fallSlow : Texture
            , fallFast : Texture
            , jumpSlow : Texture
            , jumpFast : Texture
            }
        , left :
            { stand : Texture
            , walk : Texture
            , fallSlow : Texture
            , fallFast : Texture
            , jumpSlow : Texture
            , jumpFast : Texture
            }
        }
    }


type alias GamePad =
    { left : Pressed
    , right : Pressed
    , jump : Pressed
    , run : Pressed
    , duck : Pressed
    }


type Pressed
    = NotPressed
    | StartPressed
    | HeldFor Milliseconds


type alias Milliseconds =
    Float


type alias Window =
    { width : Int
    , height : Int
    }


type Slime
    = Slime Action Direction


type Action
    = Running
    | Walking
    | Standing
    | Ducking


type Direction
    = Left
    | Right


type Msg
    = Tick Time.Posix
    | Frame Float
    | Pressed Button
    | Released Button
    | WindowSize Int Int
    | TextureLoaded (Maybe Texture)


type Button
    = GoLeft
    | GoRight
    | Duck
    | Jump
    | Run


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : () -> ( Model, Cmd Msg )
init () =
    ( { x = 0
      , y = 0
      , vx = 0
      , vy = 0
      , isGrounded = True
      , window = { width = 800, height = 600 }
      , gamepad =
            { left = NotPressed
            , right = NotPressed
            , jump = NotPressed
            , run = NotPressed
            , duck = NotPressed
            }
      , slime = Animator.init (Slime Standing Right)
      , sprites = Loading
      }
    , Browser.Dom.getViewport
        |> Task.attempt
            (\viewportResult ->
                case viewportResult of
                    Ok viewport ->
                        WindowSize
                            (round viewport.scene.width)
                            (round viewport.scene.height)

                    Err _ ->
                        WindowSize
                            (round 800)
                            (round 600)
            )
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick newTime ->
            ( model
                |> Animator.update newTime animator
            , Cmd.none
            )

        Frame dt ->
            ( model
                |> holdButtons dt
                |> gravity (dt / 10)
                |> jump model.gamepad
                |> walk model.gamepad
                |> physics (dt / 10)
                |> updateSprites
            , Cmd.none
            )

        Pressed button ->
            ( { model
                | gamepad =
                    applyButtonToGamepad button True model.gamepad
              }
            , Cmd.none
            )

        Released button ->
            ( { model
                | gamepad =
                    applyButtonToGamepad button False model.gamepad
              }
            , Cmd.none
            )

        WindowSize width height ->
            ( { model
                | window =
                    { width = width
                    , height = height
                    }
              }
            , Cmd.none
            )

        TextureLoaded Nothing ->
            ( { model | sprites = Failure }, Cmd.none )

        TextureLoaded (Just texture) ->
            ( { model
                | sprites =
                    Loaded
                        { player =
                            { right =
                                { stand = spriteFromTexture 0 0 texture
                                , walk = spriteFromTexture 1 0 texture
                                , jumpSlow = spriteFromTexture 0 0 texture
                                , jumpFast = spriteFromTexture 0 1 texture
                                , fallSlow = spriteFromTexture 0 2 texture
                                , fallFast = spriteFromTexture 0 3 texture
                                }
                            , left =
                                { stand = spriteFromTexture 0 32 texture
                                , walk = spriteFromTexture 1 32 texture
                                , jumpSlow = spriteFromTexture 0 33 texture
                                , jumpFast = spriteFromTexture 1 33 texture
                                , fallSlow = spriteFromTexture 2 33 texture
                                , fallFast = spriteFromTexture 3 33 texture
                                }
                            }
                        }
              }
            , Cmd.none
            )


spriteSheetCellSize : number
spriteSheetCellSize =
    16


spriteSheetCellSpace : number
spriteSheetCellSpace =
    0


spriteFromTexture : Float -> Float -> Texture -> Texture
spriteFromTexture x y =
    Canvas.Texture.sprite
        { x = x * (spriteSheetCellSize + spriteSheetCellSpace)
        , y = y * (spriteSheetCellSize + spriteSheetCellSpace)
        , width = spriteSheetCellSize
        , height = spriteSheetCellSize
        }


{-| -}
subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Animator.toSubscription Tick model animator
        , Browser.Events.onResize WindowSize
        , Browser.Events.onKeyDown (Decode.map Pressed decodeButton)
        , Browser.Events.onKeyUp (Decode.map Released decodeButton)
        , Browser.Events.onAnimationFrameDelta Frame
        ]


animator : Animator.Animator Model
animator =
    Animator.animator
        -- we tell the animator how to get the checked timeline using .checked
        -- and we tell the animator how to update that timeline with updateChecked
        |> Animator.watching .slime (\slime m -> { m | slime = slime })


decodeButton : Decode.Decoder Button
decodeButton =
    Decode.andThen toButton
        (Decode.field "key" Decode.string)


toButton : String -> Decode.Decoder Button
toButton string =
    case string of
        "ArrowLeft" ->
            Decode.succeed GoLeft

        "a" ->
            Decode.succeed GoLeft

        "ArrowRight" ->
            Decode.succeed GoRight

        "d" ->
            Decode.succeed GoRight

        " " ->
            Decode.succeed Jump

        "ArrowDown" ->
            Decode.succeed Duck

        "s" ->
            Decode.succeed Duck

        "Shift" ->
            Decode.succeed Run

        _ ->
            Decode.fail "Skip"


holdButtons : Float -> Model -> Model
holdButtons dt model =
    let
        gamepad =
            model.gamepad
    in
    { model | gamepad = holdButtonsOnGamepad dt gamepad }


holdButtonsOnGamepad : Milliseconds -> GamePad -> GamePad
holdButtonsOnGamepad dt gamepad =
    { left = hold dt gamepad.left
    , right = hold dt gamepad.right
    , jump = hold dt gamepad.jump
    , run = hold dt gamepad.run
    , duck = hold dt gamepad.duck
    }


hold : Milliseconds -> Pressed -> Pressed
hold dt pressed =
    case pressed of
        NotPressed ->
            NotPressed

        StartPressed ->
            HeldFor dt

        HeldFor existingDt ->
            HeldFor (existingDt + dt)


applyButtonToGamepad : Button -> Bool -> GamePad -> GamePad
applyButtonToGamepad button pressed gamepad =
    case button of
        GoLeft ->
            { gamepad
                | left =
                    if pressed then
                        StartPressed

                    else
                        NotPressed
            }

        GoRight ->
            { gamepad
                | right =
                    if pressed then
                        StartPressed

                    else
                        NotPressed
            }

        Duck ->
            { gamepad
                | duck =
                    if pressed then
                        StartPressed

                    else
                        NotPressed
            }

        Jump ->
            { gamepad
                | jump =
                    if pressed then
                        StartPressed

                    else
                        NotPressed
            }

        Run ->
            { gamepad
                | run =
                    if pressed then
                        StartPressed

                    else
                        NotPressed
            }


textures : List (Canvas.Texture.Source Msg)
textures =
    [ Canvas.Texture.loadFromImageUrl "./assets/slime/slime-sprites.png" TextureLoaded
    ]


view : Model -> Browser.Document Msg
view model =
    { title = "Slim the Slime"
    , body =
        [ Canvas.toHtmlWith
            { width = canvasSize
            , height = canvasSize
            , textures = textures
            }
            []
            (Canvas.shapes
                [ Canvas.Settings.fill (Color.rgb 0.85 0.92 1) ]
                [ Canvas.rect ( 0, 0 ) canvasSize canvasSize ]
                :: (case model.sprites of
                        Loading ->
                            [ renderText "Loading sprite sheet" ]

                        Loaded ss ->
                            renderSprites model ss

                        Failure ->
                            [ renderText "Failed to load sprite sheet!" ]
                   )
            )
        ]
    }


renderSprites : Model -> Sprites -> List Renderable
renderSprites model sprites =
    (\(Slime action direction) ->
        let
            -- this is where we decide to show the left or the right sprite.
            --
            frame pose =
                case direction of
                    Left ->
                        Animator.frame (pose sprites.player.left)

                    Right ->
                        Animator.frame (pose sprites.player.right)
        in
        if model.isGrounded then
            case action of
                -- for these first three states, we only have a single frame we care about.
                Standing ->
                    frame .stand

                Ducking ->
                    frame .stand

                Walking ->
                    -- when we're in a `Walking` state, we want to cycle through 3 frames.
                    -- And we can also specify our frames per secton
                    Animator.framesWith
                        -- `transition` are the frames we'd want to take when transitioning to this state.
                        { transition = frame .stand

                        -- `resting` is what we want to do while we're in this state.
                        , resting =
                            Animator.cycle
                                (Animator.fps 15)
                                [ frame .walk
                                , frame .stand
                                ]
                        }

                Running ->
                    -- In order to make Slim go faster, we're upping the fps
                    -- and we're also changing the frames so that he puts his arms out.
                    -- Animator.framesWith
                    --     { transition = frame sprite.tail.standArms
                    --     , resting =
                    --         Animator.cycle
                    --             (Animator.fps 30)
                    --             [ frame sprite.tail.runStep1
                    --             , frame sprite.tail.runStep2
                    --             , sprites.player
                    --             ]
                    --     }
                    Debug.todo ""

        else if model.vy > jumpForce * 0.85 then
            frame .jumpFast

        else if model.vy > 0 then
            frame .jumpSlow

        else if model.vy < jumpForce * -0.85 then
            frame .fallFast

        else if model.vy < 0 then
            frame .fallSlow

        else
            frame .stand
    )
        |> Animator.step model.slime
        |> Canvas.texture [] ( model.x, model.y )
        |> List.singleton


canvasSize : number
canvasSize =
    spriteSheetCellSize * 8


renderText : String -> Renderable
renderText txt =
    Canvas.text
        [ Canvas.Settings.Text.font { size = 48, family = "sans-serif" }
        , Canvas.Settings.Text.align Canvas.Settings.Text.Center
        , Canvas.Settings.Text.maxWidth canvasSize
        ]
        ( canvasSize / 2, canvasSize / 2 - 24 )
        txt


isHeld : Pressed -> Bool
isHeld pressed =
    case pressed of
        NotPressed ->
            False

        StartPressed ->
            True

        HeldFor _ ->
            True


walk : GamePad -> Model -> Model
walk pad model =
    let
        run yes x =
            if not model.isGrounded then
                x * 0.75

            else if yes then
                x * 2.0

            else
                x

        newVx =
            if isHeld pad.left && isHeld pad.right then
                0

            else if isHeld pad.left then
                run (isHeld pad.run) -1.8

            else if isHeld pad.right then
                run (isHeld pad.run) 1.8

            else
                0
    in
    { model | vx = newVx }


jumpForce : Float
jumpForce =
    -5.0


jump : GamePad -> Model -> Model
jump pad model =
    if model.isGrounded && pad.jump == StartPressed then
        { model | vy = jumpForce }

    else
        model


gravity : Float -> Model -> Model
gravity dt model =
    let
        ( isGrounded, newVy ) =
            if model.y < 100 then
                ( False, model.vy + dt / 4 )

            else
                ( True, 0 )
    in
    { model
        | vy = newVy
        , isGrounded = isGrounded
    }


physics : Float -> Model -> Model
physics dt model =
    { model
        | x =
            (model.x + dt * model.vx)
                |> min (canvasSize - spriteSheetCellSize)
                |> max 0
        , y = max 0 (model.y + dt * model.vy)
    }


updateSprites : Model -> Model
updateSprites model =
    let
        current =
            Animator.current model.slime

        direction =
            if model.vx > 0 then
                Right

            else if model.vx < 0 then
                Left

            else
                case current of
                    Slime _ currentDirection ->
                        currentDirection

        action =
            if model.vx /= 0 then
                if abs model.vx > 2 then
                    Running

                else
                    Walking

            else if isHeld model.gamepad.duck then
                Ducking

            else
                Standing

        newSlime =
            Slime action direction
    in
    if current /= newSlime then
        { model
            | slime =
                model.slime
                    |> Animator.go Animator.immediately newSlime
        }

    else
        model
