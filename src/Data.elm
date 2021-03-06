module Data where

import List
import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Maybe exposing (..)
import Utils exposing (..)
import TileMap
import Text
import Time exposing (..)

type alias Gpx = 
    {
        id: Int
        , name: String
        , gpx: List TileMap.Gpsx
        , bbox_topLeft: (Float, Float)
        , bbox_bottomRight: (Float, Float)
        , timeSpan: (Time, Time)
    }

fullTrace : Maybe Gpx -> Color -> TileMap.Map -> Form
fullTrace gpsx colr mapp =
    case gpsx of
        Just gpsx' -> TileMap.path2 gpsx'.gpx mapp {defaultLine | color = colr, width = 8}
        _ -> Graphics.Collage.toForm empty
        
latLons trace colr = 
    let
        latLons' = List.map (\x -> showLatLon x (toCssString colr)) trace |> List.concat
            |> div [style [("overflow-y", "scroll"), ("height", "570px"), ("width", "160px")]]
    in
        div [style [("background-color", "rgba(255, 255, 255, 0.85)"), ("padding", "20px 10px 10px 20px")]]
                        (Html.span [style [("font-weight", "bold"), ("font-size", "large"), ("color", "black")]] 
                        [Html.text ("Tail: " ++ (toString <| List.length trace) ++ (if List.length trace == 50 then "+" else "") ++ " points")]
                         :: [latLons']) |> Html.toElement 180 660

head trace icn colr mapp = case (List.head trace) of
            Just g -> 
                let
                    (x, y) = TileMap.proj (g.lat, g.lon) mapp
                    p =  (icn colr 24) |> Html.toElement 24 24 |> toForm |> move (x, y)
                    xy = ("(" ++ (toString g.lat) ++ ", " ++ (toString g.lon) ++ ")" ) |> Text.fromString 
                        |> outlinedText {defaultLine | width = 1, color = colr} 
                        |> move (x + 100, y - 10)
                    t = Utils.dateTimeToString g.timestamp |> Text.fromString
                        |> outlinedText {defaultLine | width = 1, color = colr}
                        |> move (x + 100, y + 10)
                in 
                    Graphics.Collage.group [p, xy, t]
            _ -> Graphics.Element.empty |> toForm

timelyTrace: Gpx -> Time -> Int -> TileMap.Map -> Color -> (Color -> Int -> Html) -> (Form, Element)
timelyTrace gpx t tcLength mapp colr icn = 
    let
        trace' = List.filter (\g -> g.timestamp < t && t - g.timestamp <= (if tcLength == 0 then 24 else tcLength) * 60000) gpx.gpx 
                    |> List.reverse |> List.take 50
        head' = head trace' icn colr mapp
        hstE = 
            if tcLength == 0 then Graphics.Element.empty  |> toForm
            else TileMap.path trace' mapp {defaultLine | color = Color.darkGreen, width = 2}
        latLons' = latLons trace' colr
    in 
        (Graphics.Collage.group [hstE, head'], latLons')

txt str colorr size = Html.span [style [("font-weight", "bold"), ("font-size", size), ("color", colorr)]] [Html.text str]

showInfo : Gpx -> String -> Html
showInfo g colorr = 
      (div [style [("padding-left", "20px"), ("color", colorr)]] <|
            Html.span [style [("font-weight", "bold"), ("font-size", "large"), ("color", "black")]] [Html.text g.name]
            :: br [] []
            :: br [] []
            :: txt ("Start Time: " ) "black" "large"
            :: br [] []
            :: br [] []
            :: txt (g.timeSpan |> fst |> Utils.dateTimeToString) colorr "large"
            :: br [] []
            :: br [] []
            :: txt ("End Time: ") "black" "large"
            :: br [] []
            :: br [] []
            :: txt (g.timeSpan |> snd |> Utils.dateTimeToString) colorr "large"
            :: br [] []
            :: [br [] []]
            )

showLatLon : TileMap.Gpsx -> String -> List Html
showLatLon g colorr = 
            br [] []
            :: txt ("Date: " ++ dateToString g.timestamp) colorr "medium"
            :: br [] []
            :: txt ("Time: " ++ timeToString g.timestamp) colorr "medium"
            :: br [] []
            :: txt ("Lat: " ++ toString g.lat) colorr "medium"
            :: br [] []
            :: txt ("Lon: " ++ toString g.lon) colorr "medium"
            :: [br [] []]
        