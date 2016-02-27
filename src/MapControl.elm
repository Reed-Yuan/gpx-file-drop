module MapControl where

import TileMap
import Signal.Extra exposing (..)
import Drag exposing (..)
import Data

type alias MouseWheel = 
    {
        pos: (Int, Int),
        delta: Int
    }

type MapOps = Zoom Int | Pan (Int, Int) | Size (Int, Int) | NoOp | Center (Float, Float)

mapOps : Signal.Mailbox MapOps
mapOps = Signal.mailbox NoOp

ops : Signal MouseWheel -> Signal (Int, Int) -> Signal Drag.MouseEvent -> Signal Bool -> Signal (List Data.Gpx) -> Signal MapOps
ops mouseWheelIn screenSizeIn mouseEvt shadowSg dataInSg = 
    let
        level x = if x < 0 then -1
                    else if x == 0 then 0
                    else 1
        zooms = (\ms -> ms.delta |> level |> Zoom) <~ mouseWheelIn
        sizing = (\(x, y) -> Size (x, y)) <~ screenSizeIn
        mouseDrag evt shadowed = 
            if shadowed then NoOp
            else
                case evt of
                    MoveFromTo (x0,y0) (x1, y1) -> Pan (x1 - x0, y1 - y0)
                    _ -> NoOp
        pan = Signal.map2 mouseDrag mouseEvt shadowSg |> Signal.dropRepeats
        cts = 
            let
                changed lst = 
                    case List.head lst of
                    Just gpx ->
                        let
                            x0 = (fst gpx.bbox_topLeft - fst gpx.bbox_bottomRight)/2 + fst gpx.bbox_bottomRight
                            y0 = (snd gpx.bbox_topLeft - snd gpx.bbox_bottomRight)/2 + snd gpx.bbox_bottomRight
                            d = Debug.log "(x0, y0)" (x0, y0)
                        in
                            Center (y0, x0)
                    _ -> NoOp
            in        
                Signal.map changed (Signal.dropRepeats dataInSg )
    in
        Signal.mergeMany [zooms, sizing, pan, cts]
        
trans : MapOps -> TileMap.Map -> TileMap.Map
trans op mapp = 
    case op of
        Zoom z -> TileMap.zoom mapp z
        Pan (dx, dy) -> TileMap.panPx mapp (dx, dy)
        Size (x, y) -> {mapp | size = (x, y)}
        Center (x, y) -> {mapp | center = (x, y)}
        _ -> mapp

mapSg : Signal MouseWheel -> Signal (Int, Int) -> Signal Drag.MouseEvent -> Signal Bool -> Signal (List Data.Gpx) -> Signal TileMap.Map        
mapSg mouseWheelIn screenSizeIn mouseEvt shadowSg dataInSg = 
    let
        initMap = { size = (TileMap.tileSize, TileMap.tileSize), center = (43.83488, -79.5257), zoom = 13 }
    in
        Signal.foldp trans initMap (ops mouseWheelIn screenSizeIn mouseEvt shadowSg dataInSg)
