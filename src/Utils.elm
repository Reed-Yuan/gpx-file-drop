module Utils where

import Date
import Date.Format
import Date.Create
import Date.Config.Config_en_us exposing (..)
import Vendor
import Color exposing (Color, toHsl, hsla, toRgb, rgba)
import String
import Signal.Extra
import Task
import Time

dropRepeats : List comparable -> List comparable
dropRepeats lst = 
    let
        step x (mature, t) = 
            case t of
                Nothing -> ([], Just x)
                Just y -> if x == y 
                          then (mature, Just x) 
                          else ((Just y) :: mature, Just x)
    in            
        List.foldr step ([], Nothing) lst 
        |> (\x -> snd x :: fst x)
        |> List.filterMap identity

global_tzone : Float
global_tzone = Date.fromTime 0 |> Date.Create.getTimezoneOffset |> (*) 60000 |> (-) 0 |> toFloat

timeFromString : String -> Time.Time
timeFromString str =  
    let
        t = str |> Date.fromString |> Result.withDefault (Date.fromTime 0) |> Date.toTime
    in 
        if Vendor.prefix == Vendor.Webkit
        then t 
        else t + global_tzone
        
timeToString : Float -> String
timeToString t = Date.fromTime t |> Date.Format.formatUtc Date.Config.Config_en_us.config "%H:%M:%S"
        
dateTimeToString : Float -> String
dateTimeToString t = Date.fromTime t |> Date.Format.formatUtc Date.Config.Config_en_us.config "%Y-%m-%d %H:%M:%S UTC"

toCssString : Color -> String
toCssString cl =
    let
        { red, green, blue, alpha } = toRgb cl
        rgba =
            [ (toFloat red), (toFloat green), (toFloat blue), alpha ]
                |> List.map toString
                |> (String.join ",")
    in
        "rgba(" ++ rgba ++ ")"
        
zip5
    :  Signal a
    -> Signal b
    -> Signal c
    -> Signal d
    -> Signal e
    -> Signal (a, b, c, d, e)
zip5 sga sgb sgc sgd sge = 
    let
       tmp  = Signal.Extra.zip4 sga sgb sgc sgd
       step = \(a, b, c, d) e -> (a, b, c, d, e)
    in
       Signal.map2 step tmp sge

doAll : List (Task.Task x a) -> Task.Task never (List x, List a)
doAll tasks =
    let
        tasks' = List.map (Task.map (\y -> ([], [y]))) tasks
        step : Task.Task x (List x, List a) -> Task.Task never (List x, List a) -> Task.Task never (List x, List a)
        step tsk accTsks = 
            let
                failed_ x = Task.map (\(e, k) -> (x :: e, k)) accTsks
                succeed_ (e_, k_) = Task.map (\(e, k) -> (List.append e_ e, List.append k_ k)) accTsks
            in
                Task.andThen (Task.onError tsk failed_) succeed_
    in
        List.foldl step (Task.succeed ([], [])) tasks'