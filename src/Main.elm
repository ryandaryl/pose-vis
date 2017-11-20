port module Main exposing (..)

import Html exposing (div, program, Html, input, label)
import Html.Attributes exposing (class, id, value, name, for, type_, checked)
import Html.Events exposing (onInput, onClick)
import Json.Decode exposing (string, decodeString)
import Lazy.List exposing (cycle, fromList)
import List exposing (map, filter, map2, drop, concat, length)
import Maybe exposing (withDefault)
import PoseProtocol exposing (..)
import Random exposing (generate)
import Random.List exposing (shuffle)
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


port changeMqttTopic : String -> Cmd msg


port changeMqttClient : String -> Cmd msg


port toggleHeads : Bool -> Cmd msg


{-| Hardcoded constant colours that I've arbitrarily decided on.
-}
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
    , mqttClient : String
    , showHeads : Bool
    }


init : ( Model, Cmd Msg )
init =
    ( Model [] "pose/output" "mqtt://zeus.local:1884" True, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ div
            []
            [ label [ for "mqtt-client" ] [ text "MQTT Client: " ]
            , input
                [ class ""
                , name "mqtt-client"
                , onInput UpdateMqttClient
                , value model.mqttClient
                ]
                []
            , label [ for "in-topic" ] [ text "In Topic: " ]
            , input
                [ class ""
                , name "in-topic"
                , onInput UpdateInTopic
                , value model.inTopic
                ]
                []
            , label [ for "toggle-heads" ] [ text "Show Heads: " ]
            , input
                [ class ""
                , type_ "checkbox"
                , name "toggle-heads"
                , onClick (ToggleHeads (not model.showHeads))
                , checked model.showHeads
                ]
                []
            ]
        , div [] [ renderPoses model ]
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        UpdateMqttClient client ->
            ( { model | mqttClient = client }, changeMqttClient client )

        UpdateInTopic topic ->
            ( { model | inTopic = topic }, changeMqttTopic topic )

        ToggleHeads sh ->
            ( { model | showHeads = sh }, toggleHeads sh )

        Message json ->
            ( { model | poses = (decodePoses json) }, Cmd.none )


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
    | UpdateMqttClient String
    | ToggleHeads Bool


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
