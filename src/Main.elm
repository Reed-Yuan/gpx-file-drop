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
  
port vehicleIn : Signal (List Data.Gpx)

port txtOut : Signal (List ( String, String ))
port txtOut = txtOutMbx.signal

txtOutMbx : Signal.Mailbox (List (String, String))
txtOutMbx = Signal.mailbox []

errorMbx : Signal.Mailbox (List ( String, Error ))
errorMbx = Signal.mailbox []

port txtOutTsk : Signal (Task a ())
port txtOutTsk = 
    let
        perf fileTsk = (Task.map snd fileTsk) `Task.andThen` Signal.send txtOutMbx.address
    in
        Signal.map perf parseTxtSg

port errorTsk : Signal (Task a ())
port errorTsk = 
    let
        perf fileTsk = (Task.map fst fileTsk) `Task.andThen` Signal.send errorMbx.address
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

startTimeSg = 
    let
        getStartTime gpx = 
            case (List.head gpx) of
                Just gpx' -> fst gpx'.timeSpan
                _ -> 0
    in
        Signal.map getStartTime vehicleIn
        
timeDeltaSg =
    let
        getTimeSpan gpx = 
            case (List.head gpx) of
                Just gpx' -> (snd gpx'.timeSpan) - (fst gpx'.timeSpan)
                _ -> 0
    in
        Signal.map getTimeSpan vehicleIn

--port runner : Signal (Task x ())
--port runner = VideoControl.videoRewindTaskSg startTimeSg timeDeltaSg

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

errorsSg = 
    let
        err1 = Signal.map (fst >> (\x -> if List.isEmpty x then Nothing else List.map toString x |> Just)) parseTasksSg
        err2 = Signal.map (\z -> if List.isEmpty z then Nothing else List.map (\(x, y) -> (toString y) ++ ": " ++ x) z |> Just) errorMbx.signal
    in
        Signal.mergeMany [err1, err2] |> Signal.Extra.filter [""]

parseTasksSg = 
    let
        parseSingle nativeFile (errs, acts) =
                if nativeFile.size > 1000000 
                then Result.Err ("File too big (> 1 M):" ++ nativeFile.name) |>  (\x -> (x :: errs, acts))
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
    
viewSgA = Signal.map3 initialView screenSizeIn dropZoneStatusSg errorsSg
        
initialView (w, h) dropFileModel errs = 
    collage w h [
        (layers [initMap w h, spacer 20 1 `beside` title, spacer 20 1 `beside` myLink `beside` spacer (w - 520) 1 `beside` gitLink `below` spacer 1 (h - 60)]) |> toForm
        , dropZoneView dropFileMbx.address dropFileModel |> Html.toElement 800 600 |> toForm
        , flow down (List.map (Text.fromString >> Text.height 22 >> Text.color Color.red >> leftAligned) errs) |> toForm |> moveY -80]

gitLink =
        let
            a = Text.fromString "Source code @GitHub" |> Text.link "https://github.com/Reed-Yuan/gpx-file-drop" |> Text.height 22 |> leftAligned
            b = spacer 240 40 |> color white |> opacity 0.85
        in
            layers [b, (spacer 20 1) `beside` a `below` (spacer 1 10)]

myLink =
        let
            a = Text.fromString "Developer: Reed Yuan" |> Text.link "https://ca.linkedin.com/in/reed-yuan-7b106b4" |> Text.height 22 |> leftAligned
            b = spacer 240 40 |> color white |> opacity 0.85
        in
            layers [b, (spacer 20 1) `beside` a `below` (spacer 1 10)]
            
title = Html.span [style [("color", "blue"), ("font-size", "xx-large")]] [Html.text "GPX Visualization with ELM: Drag'n Drop"] 
        |> Html.toElement 700 60
        
initMap w h = TileMap.loadMap { size = (w, h), center = (38.847399, -101.009422), zoom = 5 }        

gpxView = 
    let
        mapNetSg = mapSg mouseWheelIn screenSizeIn Drag.mouseEvents mergedShadow vehicleIn
        dataSg = Signal.map2 (\gps mapp -> let gps' = List.head gps in (gps', Data.fullTrace gps' Color.red mapp)) vehicleIn mapNetSg
        videoOptionSg = VideoControl.videoOption startTimeSg timeDeltaSg
    in
        Signal.map3 (\x y z -> x y z) (Signal.map5 render mapNetSg videoOptionSg dataSg vehicleOptionsSg hideCtlSg) showWarnMbx.signal browserIn

hideCtlSg =
    let
        hideVehiclesSg = Signal.foldp (\_ b -> not b) False hideVehiclesMbx.signal            
        hideInfoSg = Signal.foldp (\_ b -> not b) False hideInfoMbx.signal
    in
        Signal.Extra.zip hideVehiclesSg hideInfoSg
    
render : TileMap.Map -> VideoOptions -> (Maybe Data.Gpx, Form) -> VehicleOptions -> (Bool, Bool) -> Bool -> String -> Element
render  mapp videoOptions (data_, fullTrace_) vehicleOptions (hideVehicles, hideInfo) showWarn browserType = 
    case data_ of
        Just data ->
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

                baseMap = TileMap.loadMap mapp
                anologClock_ = videoOptions.anologClock |> move ((toFloat w)/2 - 280, (toFloat h)/2 - 70)
                digitClock_ = videoOptions.digitClock |> toForm |> move ((toFloat w)/2 - 100, (toFloat h)/2 - 50)
                progressBar_ = videoOptions.progressBar |> toForm |> move (0, 40 - (toFloat h)/2)

                traceWithInfo = Data.timelyTrace data videoOptions.time tl mapp Color.blue FontAwesome.reddit_alien
                
                gpxInfo = Data.showInfo data (Utils.toCssString Color.blue) |> Html.toElement 160 240
                
                vehicleInfo =
                    let
                        icn = (if hideInfo then FontAwesome.arrow_down white 20 else FontAwesome.arrow_up white 20) |> Html.toElement 20 20
                        switch = layers [spacer 180 20 |> color grey |> Graphics.Element.opacity 0.5, spacer 90 20 `beside` icn]  
                                     |> Graphics.Input.clickable (Signal.message hideInfoMbx.address ())
                        info_ = snd traceWithInfo 
                                
                        view = (if hideInfo then switch else switch `above` info_)
                   in
                        view |> toForm |> move ((toFloat w)/2 - 100, if hideInfo then 350 else 20)
                vehicleStateView_ = 
                    let
                        bck = spacer 160 600 |> color white |> opacity 0.85
                        vehicleStateView = layers [bck, 
                                            mapAlpha `below` (spacer 1 10) 
                                            `below` tailLength `below` (spacer 1 10) 
                                            `below` traceAlpha `below` (spacer 1 10)
                                            `below` (fst videoOptions.speedCtl) `below` (spacer 1 20)
                                            `below` gpxInfo `below` (spacer 1 20)
                                            ]
                        icn = (if hideVehicles then FontAwesome.arrow_down white 20 else FontAwesome.arrow_up white 20) |> Html.toElement 20 20
                        switch = layers [spacer 160 20 |> color grey |> Graphics.Element.opacity 0.5, spacer 70 20 `beside` icn]  
                                     |> Graphics.Input.clickable (Signal.message hideVehiclesMbx.address ())
                        view = (if hideVehicles then switch else switch `above` vehicleStateView)
                    in 
                        view |> toForm |> move (100 - (toFloat w)/2, if hideVehicles then 340 else 40)
                        
            in
                collage w h [toForm baseMap |> alpha malpha, fullTrace_ |> alpha talpha, fst traceWithInfo, 
                     anologClock_, digitClock_, popA, progressBar_, vehicleStateView_, 
                     vehicleInfo, title |> toForm |> move (380 - (toFloat w)/2,  (toFloat h)/2 - 40), 
                     myLink |> toForm |> move (140 - (toFloat w)/2, 45 - (toFloat h)/2),
                     gitLink |> toForm |> move ((toFloat w)/2 - 140, 45 - (toFloat h)/2)
                     ]
        _ -> Graphics.Element.empty
        
dropZoneStatusSg : Signal DropZone.Model
dropZoneStatusSg = Signal.foldp DropZone.update DropZone.init dropFileMbx.signal

dropZoneView :  Signal.Address (DropZone.Action a) -> DropZone.Model -> Html
dropZoneView address dropZoneModel =
  div
    (renderZoneAttributes address dropZoneModel)
    [h1 [style [("padding-left", "120px")]] [ Html.text "Drop a valid GPX file in this box" ]]

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
        , ("padding", "280px 40px 280px 60px")
        ]
        
dropZoneHover =
    style
        [ ( "height", "80px")
        , ( "border-radius", "10px")
        , ( "color", "red")
        , ( "border", "6px dashed red")
        , ("padding", "280px 40px 280px 60px")
        ]
            