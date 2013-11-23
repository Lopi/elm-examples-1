-- Copyright Grzegorz Balcerek 2013 www.grzegorzbalcerek.net
-- elm -r elm-runtime.js SignalsWindow.elm

import Window

showsignals a b c =
 flow down <| map plainText
 [ "Window.dimensions: " ++ show a
 , "Window.width: " ++ show b
 , "Window.height: " ++ show c
 ]

main = showsignals <~ Window.dimensions
                    ~ Window.width
                    ~ Window.height
