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
        Just gpsx' -> TileMap.path2 gpsx'.gpx mapp {defaultLine | color = colr, width = 10}
        _ -> Graphics.Collage.toForm empty

timelyTrace: Gpx -> Time -> Int -> TileMap.Map -> Color -> (Color -> Int -> Html) -> (Form, Element)
timelyTrace gpx t tcLength mapp colr icn = 
    let
        vehicleInfo = showInfo gpx (toCssString Color.blue) |> Html.toElement 160 160
        trace' = List.filter (\g -> g.timestamp < t && t - g.timestamp <= (if tcLength == 0 then 24 else tcLength) * 60000) gpx.gpx |> List.reverse
        emptyForm = Graphics.Element.empty  |> toForm
        head = case (List.head trace') of
            Just g -> 
                let
                    (x, y) = TileMap.proj (g.lat, g.lon) mapp
                    p =  (icn Color.blue 24) |> Html.toElement 24 24 |> toForm |> move (x, y)
                    n = g.vehicleName |> Text.fromString 
                        |> outlinedText {defaultLine | width = 1, color = Color.blue} 
                        |> move (x + 40, y + 20)
                    latLon = showLatLon g (toCssString Color.blue) |> Html.toElement 160 160
                in 
                    (Graphics.Collage.group [p, n], vehicleInfo `above` latLon)
            _ -> (emptyForm, vehicleInfo)
        hstE = 
            if tcLength == 0 then emptyForm
            else TileMap.path trace' mapp {defaultLine | color = Color.green, width = 2}
    in 
        (Graphics.Collage.group [hstE, fst head], snd head)

txt str colorr = Html.span [style [("font-size", "large"), ("color", colorr)]] [Html.text str]

showInfo : Gpx -> String -> Html
showInfo g colorr = 
      (div [style [("padding-left", "20px"), ("color", colorr), ("background-color", "rgba(255, 255, 255, 0.85)")]] <|
            txt g.name colorr
            :: br [] []
            :: br [] []
            :: txt ("Start Time: " ) colorr
            :: br [] []
            :: txt ("Lat: ") colorr
            :: br [] []
            :: txt ("End Time: ") colorr
            :: br [] []
            :: txt ("Speed: ") colorr
            :: br [] []
            :: [br [] []]
            )

showLatLon : TileMap.Gpsx -> String -> Html
showLatLon g colorr = 
      (div [style [("padding-left", "20px"), ("color", colorr), ("background-color", "rgba(255, 255, 255, 0.85)")]] <|
            txt "Current" colorr
            :: br [] []
            :: br [] []
            :: txt ("Time: " ++ timeToString g.timestamp) colorr
            :: br [] []
            :: txt ("Lat: " ++ toString g.lat) colorr
            :: br [] []
            :: [txt ("Lon: " ++ toString g.lon) colorr]
            )
        --|> (Html.toElement 160 160)
        