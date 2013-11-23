
import Mouse
import JavaScript
import Signal
import Char
import Graphics.Input (hoverable)
import Window
import String
import Text

buttonColor = rgb 199 235 243

data ButtonSize = Regular | Large

makeButton label btnSize =
  let xSize = case btnSize of { Regular -> 60 ; Large -> 120 }
  in collage xSize 60 [
            filled buttonColor <| rect (xSize-8) 52,
            outlined (solid black) <| rect (xSize-8) 52,
            outlined (solid black) <| rect (xSize-10) 50,
            toForm (text . bold . Text.height 30 . toText <| label)]

isMouseGoingDown = keepIf id False Mouse.isDown

makeButtonAndSignal label btnSize =
  let
    (btn, sig) = hoverable (makeButton label btnSize)
    sig2 = sampleOn isMouseGoingDown sig
    signal = keepWhen sig2 label (constant label)
  in (btn, signal)

data CalculatorState = Initial | State Float String String

display state =
  let showState state = plainText (
   case state of
    Initial -> ""
    State number op str -> if str == "" then show number else str
  )
  in collage 240 60 [
       outlined (solid black) <| rect 232 50,
       outlined (solid black) <| rect 230 48,
       toForm (container 220 50 midRight (showState state)) ]

(button0,     button0Signal)     = makeButtonAndSignal "0" Regular
(button1,     button1Signal)     = makeButtonAndSignal "1" Regular
(button2,     button2Signal)     = makeButtonAndSignal "2" Regular
(button3,     button3Signal)     = makeButtonAndSignal "3" Regular
(button4,     button4Signal)     = makeButtonAndSignal "4" Regular
(button5,     button5Signal)     = makeButtonAndSignal "5" Regular
(button6,     button6Signal)     = makeButtonAndSignal "6" Regular
(button7,     button7Signal)     = makeButtonAndSignal "7" Regular
(button8,     button8Signal)     = makeButtonAndSignal "8" Regular
(button9,     button9Signal)     = makeButtonAndSignal "9" Regular
(buttonEq,    buttonEqSignal)    = makeButtonAndSignal "=" Regular
(buttonPlus,  buttonPlusSignal)  = makeButtonAndSignal "+" Regular
(buttonMinus, buttonMinusSignal) = makeButtonAndSignal "-" Regular
(buttonDiv,   buttonDivSignal)   = makeButtonAndSignal "/" Regular
(buttonMult,  buttonMultSignal)  = makeButtonAndSignal "*" Regular
(buttonDot,   buttonDotSignal)   = makeButtonAndSignal "." Regular
(buttonC,     buttonCSignal)     = makeButtonAndSignal "C" Large
(buttonCE,    buttonCESignal)    = makeButtonAndSignal "CE" Large

lastButtonClicked = merges [
  button0Signal,
  button1Signal,
  button2Signal,
  button3Signal,
  button4Signal,
  button5Signal,
  button6Signal,
  button7Signal,
  button8Signal,
  button9Signal,
  buttonEqSignal,
  buttonPlusSignal,
  buttonMinusSignal,
  buttonDivSignal,
  buttonMultSignal,
  buttonDotSignal,
  buttonCSignal,
  buttonCESignal]

view value (w,h) = container w h middle <|
  layers [
    collage 250 370 [
      outlined (solid black) <| rect 250 370,
      outlined (solid black) <| rect 248 368,
      outlined (solid black) <| rect 246 366],
    flow down [ spacer 250 5
              , flow right [spacer 5 60, display value]
              , flow right [spacer 5 60, buttonCE, buttonC]
              , flow right [spacer 5 60, buttonPlus,  button1, button2, button3]
              , flow right [spacer 5 60, buttonMinus, button4, button5, button6]
              , flow right [spacer 5 60, buttonMult,  button7, button8, button9]
              , flow right [spacer 5 60, buttonDiv,   button0, buttonDot, buttonEq]
--              , asText value
              ]]

step btn state =
  let 
    isDigitString a = a == "." || (Char.isDigit . maybe 'x' fst . String.uncons) a
    isOper btn = btn == "+" || btn == "-" || btn == "*" || btn == "/" || btn == "="
    calculate number op str =
      let number2 = maybe 0.0 id (readFloat str)
      in if op == "+" then number + number2
         else if op == "-" then number - number2
         else if op == "*" then number * number2
         else if op == "/" then number / number2
         else number2
  in case state of {
    State prevNum op str ->
      if btn == "C" then State 0.0 "+" "" 
      else if btn == "CE" then State prevNum op "0" 
      else if str == "" && isOper btn then State prevNum btn "" 
      else if isOper btn then State (calculate prevNum op str) btn "" 
      else if isDigitString btn then State prevNum op (if (str == "" || str == "0") && btn == "." then "0."
                                                       else if str == "" || str == "0" then btn
                                                       else if (String.length str) >= 18 then str
                                                       else if btn == "." && any (\c -> c == '.') (String.toList str) then str
                                                       else str ++ btn)
      else state }

stateSignal = foldp step (State 0.0 "+" "") lastButtonClicked

main = lift2 view stateSignal Window.dimensions

