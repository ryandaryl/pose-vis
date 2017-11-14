port module Main exposing (..)

import Html exposing (div, program, Html)
import Json.Decode exposing (int, string, float, list, Decoder, decodeString)
import Json.Decode.Pipeline exposing (decode, required, optional)
import Lazy.List exposing (cycle, fromList)
import List exposing (map, filter, map2, drop, concat, length)
import Maybe exposing (withDefault)
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


port onMessage : (String -> msg) -> Sub msg


type alias Model =
    { json : String
    }


baseColours =
    [ "#2560fe"
    , "#a200ff"
    , "#ff4040"
    , "#6dc066"
    ]


getColourForId : Int -> String
getColourForId k =
    withDefault ("#000000") (get (k % (length baseColours)) baseColours)


init : ( Model, Cmd Msg )
init =
    ( Model "Hello!", Cmd.none )


type alias PoseMsg =
    { poses : List Pose
    }


type alias Pose =
    { joints : JointSpec
    , frame : Int
    , id : Int
    , time : Float
    }


type alias JointSpec =
    { nose : Point
    , neck : Point
    , rshoulder : Point
    , relbow : Point
    , rwrist : Point
    , lshoulder : Point
    , lelbow : Point
    , lwrist : Point
    , rhip : Point
    , rknee : Point
    , rankle : Point
    , lhip : Point
    , lknee : Point
    , lankle : Point
    , reye : Point
    , leye : Point
    , rear : Point
    , lear : Point
    }


type alias Point =
    { x : Float, y : Float, score : Float }


pointDecoder : Decoder Point
pointDecoder =
    decode Point
        |> optional "x" float 0
        |> optional "y" float 0
        |> optional "score" float 0


zeroPoint : Point
zeroPoint =
    Point 0 0 0


jointSpecDecoder : Decoder JointSpec
jointSpecDecoder =
    decode JointSpec
        |> optional "Nose" pointDecoder zeroPoint
        |> optional "Neck" pointDecoder zeroPoint
        |> optional "RShoulder" pointDecoder zeroPoint
        |> optional "RElbow" pointDecoder zeroPoint
        |> optional "RWrist" pointDecoder zeroPoint
        |> optional "LShoulder" pointDecoder zeroPoint
        |> optional "LElbow" pointDecoder zeroPoint
        |> optional "LWrist" pointDecoder zeroPoint
        |> optional "RHip" pointDecoder zeroPoint
        |> optional "RKnee" pointDecoder zeroPoint
        |> optional "RAnkle" pointDecoder zeroPoint
        |> optional "LHip" pointDecoder zeroPoint
        |> optional "LKnee" pointDecoder zeroPoint
        |> optional "LAnkle" pointDecoder zeroPoint
        |> optional "REye" pointDecoder zeroPoint
        |> optional "LEye" pointDecoder zeroPoint
        |> optional "REar" pointDecoder zeroPoint
        |> optional "LEar" pointDecoder zeroPoint


poseDecoder : Decoder Pose
poseDecoder =
    decode Pose
        |> required "joints" jointSpecDecoder
        |> required "frame" int
        |> required "id" int
        |> required "time" float


poseMsgDecoder : Decoder PoseMsg
poseMsgDecoder =
    decode PoseMsg
        |> required "poses" (list poseDecoder)


type Msg
    = NoOp
    | Message String


headImages =
    [ "img/obama-head.png"
    , "img/ada-head.png"
    , "img/haskell-head.png"
    , "img/grace-hopper-head.png"
    , "img/beyonce-head.png"
    , "img/spj-head.png"
    ]


maybeHead : Int -> Point -> List (Svg m)
maybeHead id point =
    if point.score == 0 then
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
            , x (toString (point.x - 90))
            , y (toString (point.y - 100))
            , height "200"
            ]
            []
        ]


renderSinglePose : Pose -> List (Svg m)
renderSinglePose pose =
    (bones (getColourForId pose.id) (pose.joints) ++ maybeHead pose.id pose.joints.nose)


renderPoses : Result String PoseMsg -> Html m
renderPoses rpm =
    case rpm of
        Ok pm ->
            svg [ width "1280", height "720" ]
                (concat (map renderSinglePose pm.poses))

        Err e ->
            text e


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
            map (takeWhile (\j -> (j joints).score /= 0)) paths

        lines =
            map (\js -> map2 mkLine js (drop 1 js)) drawable

        mkLine j1 j2 =
            line
                [ x1 (toString (j1 joints).x)
                , y1 (toString (j1 joints).y)
                , x2 (toString (j2 joints).x)
                , y2 (toString (j2 joints).y)
                , style ("stroke-width: 2; stroke: " ++ colour ++ ";")
                ]
                []
    in
        concat lines


jointsOnly : List Point -> List (Svg m)
jointsOnly ps =
    let
        nonZero =
            filter (\p -> p.x /= 0 && p.y /= 0) ps

        f { x, y } =
            circle [ cx (toString x), cy (toString y), r "10" ] []
    in
        map f nonZero


points : JointSpec -> List Point
points j =
    [ j.nose
    , j.neck
    , j.rshoulder
    , j.relbow
    , j.rwrist
    , j.lshoulder
    , j.lelbow
    , j.lwrist
    , j.rhip
    , j.rknee
    , j.rankle
    , j.lhip
    , j.lknee
    , j.lankle
    , j.reye
    , j.leye
    ]


view : Model -> Html Msg
view model =
    div []
        [ renderPoses (decodeString poseMsgDecoder model.json) ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        Message m ->
            ( Model m, Cmd.none )


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
