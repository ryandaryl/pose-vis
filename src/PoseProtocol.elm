module PoseProtocol exposing (..)

import Json.Decode exposing (int, float, list, Decoder)
import Json.Decode.Pipeline exposing (decode, required, optional)


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


{-| Handy function to switch from the strong datastructure to a list, so we
can easily loop over them and do things.
-}
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
