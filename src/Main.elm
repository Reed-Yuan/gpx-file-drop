module Main where 

import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Color exposing (..)
import TileMap
import FontAwesome
import Html exposing (Html, div, input, button, h1, p, text)
import Html.Attributes exposing (..)
import Time exposing (..)
import Set exposing (..)
import Text
import Graphics.Input
import Signal.Extra exposing (..)
import Drag exposing (..)
import Task exposing (..)
import String
import Effects exposing (Effects)
import StartApp
import Json.Decode as Json exposing (Value, andThen)
import FileReader exposing (FileRef, NativeFile, readAsTextFile, Error(..))
import DropZone exposing (..)
import MimeType exposing (MimeType(Text))

import Data exposing (..)
import MapControl exposing (..)
import VideoControl exposing (..)
import VehicleControl exposing (..)
import Widget
import Utils
  
global_colors : List Color
global_colors = [Color.red, Color.blue, Color.brown, Color.orange, Color.darkGreen]
global_icons : List (Color -> Int -> Html.Html)
global_icons = [FontAwesome.truck, FontAwesome.ambulance, FontAwesome.taxi, FontAwesome.motorcycle, FontAwesome.bus]

port vehicleIn : Signal (List (List TileMap.Gpsx))

port txtOut : Signal (List ( String, String ))
port txtOut = txtOutMbx.signal

txtOutMbx : Signal.Mailbox (List (String, String))
txtOutMbx = Signal.mailbox []

port txtOutTsk : Signal (Task a ())
port txtOutTsk = 
    let
        perf fileTsk = (Task.map snd fileTsk) `Task.andThen` Signal.send txtOutMbx.address
    in
        Signal.map perf parseTxtSg

parseTxtSg : Signal (Task a ( List ( String, Error ), List ( String, String ) )) 
parseTxtSg = 
    let
        parseFile (fname, fileTsks) = (Task.map ((,) fname) fileTsks) |> (mapError ((,) fname))
    in
        Signal.map (snd >> (List.map parseFile) >> Utils.doAll) parseTasksSg

port mouseWheelIn : Signal MouseWheel

port screenSizeIn : Signal (Int, Int)

port browserIn : Signal String

port runner : Signal (Task x ())
port runner = VideoControl.videoRewindTaskSg

shadowFlow : Signal.Mailbox Bool
shadowFlow = Signal.mailbox False

mergedShadow = Signal.mergeMany [VehicleControl.shadowSg, VideoControl.shadowSg] |> Signal.dropRepeats

hideVehiclesMbx : Signal.Mailbox ()
hideVehiclesMbx = Signal.mailbox ()

hideInfoMbx : Signal.Mailbox ()
hideInfoMbx = Signal.mailbox ()

showWarnMbx : Signal.Mailbox Bool
showWarnMbx = Signal.mailbox True

dropFileMbx : Signal.Mailbox (DropZone.Action (List NativeFile))
dropFileMbx = Signal.mailbox DropZone.DragLeave

parseTasksSg = 
    let
        parseSingle nativeFile (errs, acts) =
            if nativeFile.size > 500000 
            then Result.Err ("File too big (> 500K):" ++ nativeFile.name) |>  (\x -> (x :: errs, acts))
            else (nativeFile.name, readAsTextFile nativeFile.blob) |> (\x -> (errs, x :: acts))
        parseFiles fileEvt = 
            case fileEvt of
                Drop files -> List.foldl parseSingle ([], []) files
                _ -> ([], [])
    in
        Signal.map parseFiles dropFileMbx.signal

type alias Model =
    {
        window_size: (Int, Int)
        , time_span: (Time, Time)
        , bound: (Float, Float)
        , gpx : List TileMap.Gpsx
    }
    
main = Signal.Extra.switchWhen (Signal.map List.isEmpty vehicleIn) viewSgA gpxView
    
viewSgA = Signal.map2 initialView screenSizeIn dropZoneStatusSg
        
initialView (w, h) dropFileModel = 
    collage w h [
        (layers [initMap w h, title, gitLink `below` spacer 1 (h - 60)]) |> toForm
        , dropZoneView dropFileMbx.address dropFileModel |> Html.toElement 600 600 |> toForm]

gitLink =
        let
            a = Text.fromString "Source code @GitHub" |> Text.link "https://github.com/Reed-Yuan/geo-elm.git" |> Text.height 22 |> leftAligned
            b = spacer 240 40 |> color white |> opacity 0.85
        in
            layers [b, (spacer 20 1) `beside` a `below` (spacer 1 10)]
            
title = Html.span [style [("color", "blue"), ("font-size", "xx-large")]] [Html.text "GPX Visualization with ELM: Drag'n Drop"] 
        |> Html.toElement 700 60
        
initMap w h = TileMap.loadMap { size = (w, h), center = (38.847399, -101.009422), zoom = 5 }        

gpxView = 
    let
        mapNetSg = mapSg mouseWheelIn screenSizeIn Drag.mouseEvents mergedShadow
        dataSg = Signal.map4 (\gps mapp startTime timeDelta -> List.map3 (\x y z -> Data.augGps x y z mapp startTime timeDelta) gps global_colors global_icons) 
                    vehicleIn mapNetSg VideoControl.startTimeSg VideoControl.timeDeltaSg
    in
        Signal.map3 (\x y z -> x y z) (Signal.map5 render mapNetSg VideoControl.videoOptionSg dataSg vehicleOptionsSg hideCtlSg) showWarnMbx.signal browserIn

hideCtlSg =
    let
        hideVehiclesSg = Signal.foldp (\_ b -> not b) False hideVehiclesMbx.signal            
        hideInfoSg = Signal.foldp (\_ b -> not b) False hideInfoMbx.signal
    in
        Signal.Extra.zip hideVehiclesSg hideInfoSg
    
render : TileMap.Map -> VideoOptions -> List Data.VehiclTrace -> VehicleOptions -> (Bool, Bool) -> Bool -> String -> Element
render  mapp videoOptions data vehicleOptions (hideVehicles, hideInfo) showWarn browserType = 
    let
        w = mapp.size |> fst
        h = mapp.size |> snd
        
        popA =
            let
                warn =  "Your browser is " 
                            ++ browserType ++ ", some controls in \nthis demo may not be working properly, \nplease use Google Chrome for best effects !!"
                            |> Text.fromString |> Text.color yellow |> Text.height 20 |> leftAligned
                warnButton = spacer 180 1 `beside` (Graphics.Input.button (Signal.message showWarnMbx.address False) "OK"  |> Graphics.Element.size 40 24) `below` spacer 1 20
                popp = layers [spacer 440 150 |> color black |> opacity 0.5, (spacer 40 1 `beside` warn `below` spacer 1 20) `above` warnButton] 
                        |> toForm |> move (420 - (toFloat w)/2,  (toFloat h)/2 - 140)
            in
                if showWarn && not (String.startsWith "Chrome" browserType) then popp else (Graphics.Element.empty |> toForm)
        
        (traceAlpha, talpha) = vehicleOptions.traceAlpha
        (mapAlpha, malpha) = vehicleOptions.mapAlpha
        (tailLength, tl) = vehicleOptions.tailLength
        vehicleList = vehicleOptions.selectedVehicles

        baseMap = TileMap.loadMap mapp
        filteredTraces = List.filter (\(id_, _, _, _, _, _) -> Set.member id_ vehicleList) data
        anologClock_ = videoOptions.anologClock |> move ((toFloat w)/2 - 280, (toFloat h)/2 - 70)
        digitClock_ = videoOptions.digitClock |> toForm |> move ((toFloat w)/2 - 100, (toFloat h)/2 - 50)
        progressBar_ = videoOptions.progressBar |> toForm |> move (0, 40 - (toFloat h)/2)
        
        traceWithInfo = List.map (\vtrace -> showTrace vtrace videoOptions.time tl mapp) filteredTraces |> List.unzip
        vehicleTrace = fst traceWithInfo |> group
        fullTrace = List.map (\(_, _, _, _, vtrace, _) -> vtrace) filteredTraces |> group |> alpha talpha
        
        vehicleInfo =
            let
                icn = (if hideInfo then FontAwesome.arrow_down white 20 else FontAwesome.arrow_up white 20) |> Html.toElement 20 20
                switch = layers [spacer 160 20 |> color grey |> Graphics.Element.opacity 0.5, spacer 70 20 `beside` icn]  
                             |> Graphics.Input.clickable (Signal.message hideInfoMbx.address ())
                info_ = (snd traceWithInfo) |> (List.foldr above Graphics.Element.empty) 
                        |> container 160 800 (midTopAt (absolute 80) (absolute 0))
                        
                view = (if hideInfo then switch else switch `above` info_)
           in
                view |> toForm |> move ((toFloat w)/2 - 100, if hideInfo then 380 else -20)
        
        vehicleStateView_ = 
            let
                bck = spacer 160 640 |> color white |> opacity 0.85
                checkBoxes_ = checkBoxes vehicleList
                vehicleStateView = layers [bck, 
                                    mapAlpha `below` (spacer 1 10) 
                                    `below` tailLength `below` (spacer 1 10) 
                                    `below` traceAlpha `below` (spacer 1 10)
                                    `below` (fst videoOptions.speedCtl) `below` (spacer 1 10)
                                    `below` (fst videoOptions.timeDeltaCtl) `below` (spacer 1 10)
                                    `below` (fst videoOptions.startTimeCtl) `below` (spacer 1 20)
                                    `below` checkBoxes_]
                icn = (if hideVehicles then FontAwesome.arrow_down white 20 else FontAwesome.arrow_up white 20) |> Html.toElement 20 20
                switch = layers [spacer 160 20 |> color grey |> Graphics.Element.opacity 0.5, spacer 70 20 `beside` icn]  
                             |> Graphics.Input.clickable (Signal.message hideVehiclesMbx.address ())
                view = (if hideVehicles then switch else switch `above` vehicleStateView)
            in 
                view |> toForm |> move (100 - (toFloat w)/2, if hideVehicles then 360 else 40)
                
    in
        collage w h [toForm baseMap |> alpha malpha, fullTrace, vehicleTrace, 
             anologClock_, digitClock_, popA, progressBar_, vehicleStateView_, vehicleInfo]

        
dropZoneStatusSg : Signal DropZone.Model
dropZoneStatusSg = Signal.foldp DropZone.update DropZone.init dropFileMbx.signal

dropZoneView :  Signal.Address (DropZone.Action a) -> DropZone.Model -> Html
dropZoneView address dropZoneModel =
  div
    (renderZoneAttributes address dropZoneModel)
    [h1 [] [ Html.text "Drop a valid GPX file in this box" ]]

renderZoneAttributes :  Signal.Address (DropZone.Action a) -> DropZone.Model -> List Html.Attribute
renderZoneAttributes address dropZoneModel =
    ( if DropZone.isHovering dropZoneModel then
        dropZoneHover -- style the dropzone differently depending on whether the user is hovering
      else
        dropZoneDefault          
    )
    ::
    -- add the necessary DropZone event wiring 
    dropZoneEventHandlers FileReader.parseDroppedFiles dropFileMbx.address

dropZoneDefault =
    style
        [ ( "height", "80px")
        , ( "border-radius", "10px")
        , ( "color", "green")
        , ( "border", "6px dashed green")
        , ("padding", "180px 40px 180px 60px")
        ]
        
dropZoneHover =
    style
        [ ( "height", "80px")
        , ( "border-radius", "10px")
        , ( "color", "red")
        , ( "border", "6px dashed red")
        , ("padding", "180px 40px 180px 60px")
        ]
            