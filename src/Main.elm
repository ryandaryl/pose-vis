port module Main exposing (..)

import Json.Encode exposing (encode)
import Html exposing (div, program, Html, input, label, img)
import Html.Attributes exposing (class, id, value, name, for, type_, checked)
import Html.Events exposing (onInput, onClick)
import Json.Decode exposing (string, decodeString)
import List exposing (map, sort, filter, map2, drop, concat, length)
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


port setTopic : (String -> msg) -> Sub msg


port sendConfigureMessage : String -> Cmd msg


port changeMqttTopic : String -> Cmd msg


port changeMqttServer : String -> Cmd msg


{-| Hardcoded constant colours that I've arbitrarily decided on.
-}
baseColourPairs : List ( String, String )
baseColourPairs =
    [ ( "#8787ff", "#ff00d7" ) -- Purple/Pink
    , ( "#5fd7d7", "#00d75f" ) -- Blue/Green
    , ( "#ff5f5f", "#ffd787" ) -- Red/Orange
    , ( "#00ff5f", "#5fffd7" ) -- BrightGreen/WeirdGreen
    , ( "#d787ff", "#00d7ff" ) -- Purple/Blue
    ]


getColourPairForId : Int -> ( String, String )
getColourPairForId k =
    withDefault ( "#000000", "#000000" )
        (get (k % (length baseColourPairs))
            baseColourPairs
        )


type alias Model =
    { poses : List Pose
    , inTopic : String
    , mqttServer : String
    , showHeads : Bool
    , danceCrew : Bool
    , temperature : Float
    , danceFloor : Bool
    }


init : ( Model, Cmd Msg )
init =
    ( Model [] "" "mqtt://zeus.local:1884" True False 0.9 False, Cmd.none )


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
            , label [ for "dance-floor" ] [ text "Floor? " ]
            , input
                [ type_ "checkbox"
                , name "dance-floor"
                , onClick (DanceFloor (not model.danceFloor))
                , checked model.danceFloor
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

        UpdateInTopicFromJS topic ->
            ( { model | inTopic = topic }, Cmd.none )

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

        DanceFloor floor ->
            let
                newModel =
                    { model | danceFloor = floor }
            in
                ( newModel, Cmd.none )

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
doSendConfigureMessage : { a | danceCrew : Bool, temperature : Float } -> Cmd msg
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
    | UpdateInTopicFromJS String
      -- Outgoing actions for the form fields
    | UpdateInTopic String
    | UpdateMqttServer String
    | ToggleHeads Bool
    | DanceCrew Bool
    | DanceFloor Bool
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



-- Seriously no indexOf function??


helper : List a -> a -> Int -> Int
helper lst elem offset =
    case lst of
        [] ->
            0

        x :: xs ->
            if x == elem then
                offset
            else
                helper xs elem (offset + 1)


indexOf : List a -> a -> Int
indexOf lst element =
    helper lst element 0


renderSinglePose : Model -> List Float -> Pose -> List (Svg m)
renderSinglePose model necks pose =
    let
        myIndex =
            (indexOf necks (pose.joints.neck.x))
    in
        (bones (getColourPairForId myIndex) (pose.joints) ++ maybeHead model.showHeads myIndex pose.joints.nose)


index : List Pose -> List Float
index poses =
    sort (map (.joints >> .neck >> .x) poses)


{-| Just applies 'renderSinglePose' to all the poses; and puts them in a list.
Note that we've halved the display sie so things are easier to deal with.
-}
renderPoses : Model -> Html m
renderPoses model =
    svg
        [ width "640"
        , height "360"
        , Svg.Attributes.class
            (if model.danceFloor then
                "floor"
             else
                ""
            )
        ]
        (concat (map (renderSinglePose model (index model.poses)) model.poses))


bones : ( String, String ) -> JointSpec -> List (Svg m)
bones ( cLeft, cRight ) joints =
    let
        paths =
            [ map ((,) cRight) [ .nose, .reye, .rear ]
            , map ((,) cLeft) [ .nose, .leye, .lear ]
            , map ((,) "#ffafd7") [ .nose, .neck ]
            , map ((,) cRight) [ .neck, .rshoulder, .relbow, .rwrist ]
            , map ((,) cLeft) [ .neck, .lshoulder, .lelbow, .lwrist ]
            , map ((,) cRight) [ .neck, .rhip, .rknee, .rankle ]
            , map ((,) cLeft) [ .neck, .lhip, .lknee, .lankle ]
            ]

        drawable =
            map (takeWhile (\( _, j ) -> notZero (j joints))) paths

        lines =
            map (\js -> map2 mkLine js (drop 1 js)) drawable

        mkLine ( _, j1 ) ( c, j2 ) =
            line
                [ x1 (toString ((j1 joints).x / 2))
                , y1 (toString ((j1 joints).y / 2))
                , x2 (toString ((j2 joints).x / 2))
                , y2 (toString ((j2 joints).y / 2))
                , style ("stroke-width: 5; stroke: " ++ c ++ ";")
                ]
                []
    in
        concat lines


notZero : { a | score : number, x : number1, y : number2 } -> Bool
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
    Sub.batch [ onMessage Message, setTopic UpdateInTopic ]


main : Program Never Model Msg
main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
