-- Copyright Grzegorz Balcerek 2013 www.grzegorzbalcerek.net
-- elm -r elm-runtime.js SignalsMouse.elm

import Mouse

showsignals a b c d e f g h i j =
 flow down <| map plainText
 [ "Mouse.position: " ++ show a
 , "Mouse.x: " ++ show b
 , "Mouse.y: " ++ show c
 , "Mouse.clicks: " ++ show d
 , "sampleOn Mouse.clicks Mouse.position: " ++ show e
 , "Mouse.isDown: " ++ show f
 , "Mouse.isClicked: " ++ show g
 , "count Mouse.isDown: " ++ show h
 , "count Mouse.isClicked: " ++ show i
 , "sampleOn Mouse.isDown Mouse.position: " ++ show j
 ]

main = showsignals <~ Mouse.position
                    ~ Mouse.x
                    ~ Mouse.y
                    ~ Mouse.clicks
                    ~ sampleOn Mouse.clicks Mouse.position
                    ~ Mouse.isDown
                    ~ Mouse.isClicked
                    ~ count Mouse.isDown
                    ~ count Mouse.isClicked
                    ~ sampleOn Mouse.isDown Mouse.position

