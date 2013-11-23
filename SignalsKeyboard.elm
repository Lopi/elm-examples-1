-- Copyright Grzegorz Balcerek 2013 www.grzegorzbalcerek.net
-- elm -r elm-runtime.js SignalsKeyboard.elm

import Keyboard

showsignals a b c d e f g h i j k =
 flow down <| map plainText
 [ "Keyboard.keysDown: " ++ show a
 , "Keyboard.lastPressed: " ++ show b
 , "dropIf isEmpty [] Keyboard.keysDown: " ++ show c
 , "Keyboard.isDown 32: " ++ show d
 , "Keyboard.shift: " ++ show e
 , "Keyboard.enter: " ++ show f
 , "Keyboard.space: " ++ show g
 , "Keyboard.ctrl: " ++ show h
 , "Keyboard.arrows: " ++ show i
 , "Keyboard.wasd: " ++ show j
 , "Keyboard.directions 81 65 79 80: " ++ show k
 ]


main = showsignals <~ Keyboard.keysDown
                    ~ Keyboard.lastPressed
                    ~ dropIf isEmpty [] Keyboard.keysDown
                    ~ Keyboard.isDown 32
                    ~ Keyboard.shift
                    ~ Keyboard.enter
                    ~ Keyboard.space
                    ~ Keyboard.ctrl
                    ~ Keyboard.arrows
                    ~ Keyboard.wasd
                    ~ Keyboard.directions 81 65 79 80

