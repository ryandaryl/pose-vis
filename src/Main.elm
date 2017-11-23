port module Main exposing (..)

import Json.Encode exposing (encode)
import Html exposing (div, program, Html, input, label, img)
import Html.Attributes exposing (class, id, value, name, for, type_, checked)
import Html.Events exposing (onInput, onClick)
import Json.Decode exposing (string, decodeString)
import List exposing (map, filter, map2, drop, concat, length)
import Maybe exposing (withDefault)
import PoseProtocol exposing (..)
import Svg exposing (Svg, svg, circle, text, image, line)
import Utils exposing (takeWhile, get)
import Svg.Attributes
    exposing
        ( cx
        , cy
        , r
        , fill
        , width
        , height
        , xlinkHref
        , x
        , y
        , x1
        , x2
        , y1
        , y2
        , style
        )


{-| Input port for the messages coming in of the input queue.
-}
port onMessage : (String -> msg) -> Sub msg


port sendConfigureMessage : String -> Cmd msg


port changeMqttTopic : String -> Cmd msg


port changeMqttServer : String -> Cmd msg


{-| Hardcoded constant colours that I've arbitrarily decided on.
-}
baseColours : List String
baseColours =
    [ "#2560fe"
    , "#a200ff"
    , "#ff4040"
    , "#6dc066"
    ]


getColourForId : Int -> String
getColourForId k =
    withDefault ("#000000") (get (k % (length baseColours)) baseColours)


type alias Model =
    { poses : List Pose
    , inTopic : String
    , mqttServer : String
    , showHeads : Bool
    , danceCrew : Bool
    , temperature : Float
    }


init : ( Model, Cmd Msg )
init =
    ( Model [] "pose/output" "mqtt://zeus.local:1884" True False 0.8, Cmd.none )


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ div
            []
            [ label [ for "mqtt-server" ] [ text "MQTT Server: " ]
            , input
                [ name "mqtt-server"
                , onInput UpdateMqttServer
                , value model.mqttServer
                ]
                []
            , label [ for "in-topic" ] [ text "In Topic: " ]
            , input
                [ name "in-topic"
                , onInput UpdateInTopic
                , value model.inTopic
                ]
                []
            , label [ for "toggle-heads" ] [ text "Show Heads: " ]
            , input
                [ type_ "checkbox"
                , name "toggle-heads"
                , onClick (ToggleHeads (not model.showHeads))
                , checked model.showHeads
                ]
                []
            ]
        , div [] [ renderPoses model ]
        , div []
            [ label [ for "dance-crew" ] [ text "Dance Crew? " ]
            , input
                [ type_ "checkbox"
                , name "dance-crew"
                , onClick (DanceCrew (not model.danceCrew))
                , checked model.danceCrew
                ]
                []
            , label [ for "temperature" ] [ text "Temperature " ]
            , input
                [ name "temperature"
                , type_ "range"
                , Html.Attributes.min "0"
                , Html.Attributes.max "1"
                , Html.Attributes.step "0.05"
                , onInput
                    (Temperature
                        << (\s ->
                                Result.withDefault 0.8
                                    (String.toFloat s)
                           )
                    )
                , value (toString model.temperature)
                ]
                []
            , img
                [ Html.Attributes.src "img/sp.png"
                , height "30"
                , class "logo"
                ]
                []
            ]
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        UpdateMqttServer server ->
            ( { model | mqttServer = server }, changeMqttServer server )

        UpdateInTopic topic ->
            ( { model | inTopic = topic }, changeMqttTopic topic )

        ToggleHeads sh ->
            ( { model | showHeads = sh }, Cmd.none )

        Temperature temp ->
            let
                newModel =
                    { model | temperature = temp }
            in
                ( newModel
                , Cmd.batch
                    [ doSendConfigureMessage
                        newModel
                    , Cmd.none
                    ]
                )

        Message json ->
            ( { model | poses = (decodePoses json) }, Cmd.none )

        DanceCrew crew ->
            let
                newModel =
                    { model | danceCrew = crew }
            in
                ( newModel
                , doSendConfigureMessage
                    newModel
                )


{-| TODO: This should be radically cleaned up.
-}
doSendConfigureMessage model =
    let
        json =
            (encode
                0
                (configureObj
                    (ConfigureMsg
                        (Configure
                            model.danceCrew
                            model.temperature
                        )
                    )
                )
            )
    in
        sendConfigureMessage json


decodePoses : String -> List Pose
decodePoses json =
    case decodeString poseMsgDecoder json of
        Ok ps ->
            ps.poses

        Err e ->
            []


type Msg
    = NoOp
      -- Inoming MQTT Message
    | Message String
      -- Outgoing actions for the form fields
    | UpdateInTopic String
    | UpdateMqttServer String
    | ToggleHeads Bool
    | DanceCrew Bool
    | Temperature Float


headImages : List String
headImages =
    [ "img/obama-head.png"
    , "img/ada-head.png"
    , "img/haskell-head.png"
    , "img/grace-hopper-head.png"
    , "img/beyonce-head.png"
    , "img/spj-head.png"
    ]


{-| Given a neck, draw a head. If there isn't any neck, then don't draw
anything!
-}
maybeHead : Bool -> Int -> Point -> List (Svg m)
maybeHead showHeads id point =
    if point.score == 0 || not showHeads then
        []
    else
        [ image
            [ xlinkHref
                (withDefault ("img/obama-head.png")
                    (get
                        (id
                            % length
                                headImages
                        )
                        headImages
                    )
                )
            , x (toString (point.x / 2 - 30))
            , y (toString (point.y / 2 - 40))
            , height "80"
            ]
            []
        ]


renderSinglePose : Model -> Pose -> List (Svg m)
renderSinglePose model pose =
    (bones (getColourForId pose.id) (pose.joints) ++ maybeHead model.showHeads pose.id pose.joints.nose)


{-| Just applies 'renderSinglePose' to all the poses; and puts them in a list.
Note that we've halved the display sie so things are easier to deal with.
-}
renderPoses : Model -> Html m
renderPoses model =
    svg [ width "640", height "360" ]
        (concat (map (renderSinglePose model) model.poses))


bones : String -> JointSpec -> List (Svg m)
bones colour joints =
    let
        paths =
            [ [ .nose, .reye, .rear ]
            , [ .nose, .leye, .lear ]
            , [ .nose, .neck ]
            , [ .neck, .rshoulder, .relbow, .rwrist ]
            , [ .neck, .lshoulder, .lelbow, .lwrist ]
            , [ .neck, .rhip, .rknee, .rankle ]
            , [ .neck, .lhip, .lknee, .lankle ]
            ]

        drawable =
            map (takeWhile (\j -> notZero (j joints))) paths

        lines =
            map (\js -> map2 mkLine js (drop 1 js)) drawable

        mkLine j1 j2 =
            line
                [ x1 (toString ((j1 joints).x / 2))
                , y1 (toString ((j1 joints).y / 2))
                , x2 (toString ((j2 joints).x / 2))
                , y2 (toString ((j2 joints).y / 2))
                , style ("stroke-width: 5; stroke: " ++ colour ++ ";")
                ]
                []
    in
        concat lines


notZero p =
    not (p.x == 0 || p.y == 0 || p.score == 0)


jointsOnly : List Point -> List (Svg m)
jointsOnly ps =
    let
        nonZero =
            filter notZero ps

        f { x, y } =
            circle [ cx (toString x), cy (toString y), r "10" ] []
    in
        map f nonZero


subscriptions : Model -> Sub Msg
subscriptions model =
    onMessage Message


main : Program Never Model Msg
main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
