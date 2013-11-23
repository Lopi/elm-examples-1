-- Copyright Grzegorz Balcerek 2013 www.grzegorzbalcerek.net
-- elm -r elm-runtime.js SignalsTime.elm

import Time
import Mouse

showsignals a b c d e f =
 flow down <| map plainText
 [ "hour: " ++ show hour
 , "minute: " ++ show minute
 , "second: " ++ show second
 , "millisecond: " ++ show millisecond
 , "fps 2: " ++ show a
 , "fpsWhen 10 Mouse.isDown: " ++ show b
 , "every second: " ++ show c
 , "since (2*second) Mouse.clicks: " ++ show d
 , "timestamp Mouse.isDown: " ++ show e
 , "delay second Mouse.position: " ++ show f
 ]

main = showsignals <~ fps 2
                    ~ fpsWhen 10 Mouse.isDown
                    ~ every second
                    ~ since (2*second) Mouse.clicks
                    ~ timestamp Mouse.isDown
                    ~ delay second Mouse.position
