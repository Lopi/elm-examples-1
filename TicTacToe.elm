import Mouse
import Graphics.Input

-- list utils

subsequences lst = case lst of
  []  -> [[]]
  h::t -> let st = subsequences t
         in st ++ map (\x -> h::x) st

-- raw signals

(newGameButton, buttonClickedSignal) = Graphics.Input.button " New Game "
mouseClickPositionSignal = sampleOn Mouse.clicks Mouse.position

-- input signal

data Input = Ignore | ClickedField Int Int | NewGame

clickedFieldsInputSignal =
  let transform mousePosition =
          let (x,y) = mousePosition
              col = 1 + x `div` 100
              row = 1 + y `div` 100
          in
            if col >= 1 && col <= 3 && row >= 1 && row <= 3
            then ClickedField col row
            else NewGame--Ignore
  in lift transform mouseClickPositionSignal

newGameSignal = lift (\_ -> NewGame) buttonClickedSignal

inputSignal = merge clickedFieldsInputSignal newGameSignal

-- data types

data Player = O | X
other player = case player of { X -> O ; O -> X }

data Result = Draw | Winner Player

data Field = Field Int Int

data GameState = FinishedGame Result [(Field,Player)]
               | NotFinishedGame Player [(Field,Player)]

board state = case state of
  (NotFinishedGame _ board) -> board
  (FinishedGame _ board) -> board

isFieldEmpty board (Field col row) = all (\elem -> not (fst elem == Field col row)) board

-- game state

initialState = NotFinishedGame X []

gameState = foldp step initialState inputSignal

step inputEvent state =
   case inputEvent of
    Ignore -> state
    NewGame -> initialState
    ClickedField col row ->
      case state of
        FinishedGame _ _ -> state
        NotFinishedGame player board -> 
          if isFieldEmpty board (Field col row)
          then humanMove player board (col,row)
          else state

makeNewState board player continuation =
  if playerWon player board
  then FinishedGame (Winner player) board
  else if length board == 9
  then FinishedGame Draw board
  else continuation ()

humanMove player board (col, row) = 
  let newBoard = (Field col row,player)::board
  in makeNewState newBoard player (\_ -> computerMove (other player) newBoard)

computerMove player board =
  let fields = [Field 2 2,Field 1 1,Field 3 3,Field 1 3,Field 3 1,Field 1 2,Field 2 1,Field 2 3,Field 3 2]
      newField = head <| filter (isFieldEmpty board) fields
      newBoard = (newField,player)::board
  in makeNewState newBoard player (\_ -> NotFinishedGame (other player) newBoard)

-- playerWon : Player -> Board -> Bool
playerWon player =
  let
    fieldsAreInLine x =
      (let f (Field c r) = c == 1     in all f x) ||
      (let f (Field c r) = c == 2     in all f x) ||
      (let f (Field c r) = c == 3     in all f x) ||
      (let f (Field c r) = r == 1     in all f x) ||
      (let f (Field c r) = r == 2     in all f x) ||
      (let f (Field c r) = r == 3     in all f x) ||
      (let f (Field c r) = c == r     in all f x) ||
      (let f (Field c r) = c + r == 4 in all f x)
  in
    not . isEmpty .
    filter fieldsAreInLine .
    map (map fst) .
    filter (let f (_,p) = p == player in all f) .
    filter (\x -> length x == 3) . subsequences

-- view

view state = flow down [ layers [boardLines, boardFigures state]
--                       , container 300 60 middle <| plainText <| stateDescription state
                       , container 300 60 middle newGameButton
--                       , asText state
                       ]

stateDescription state = case state of
  FinishedGame Draw _ -> "Game Over. Draw"
  FinishedGame (Winner p) _ -> "Game Over. Winner: " ++ show p
  NotFinishedGame p _ -> "Next move: " ++ show p

boardLines = collage 300 300 [ filled black (rect 3 300) |> move (-50,0)
                             , filled black (rect 3 300) |> move (50,0)
                             , filled black (rect 300 3) |> move (0,-50)
                             , filled black (rect 300 3) |> move (0,50) ]

figureElement player = collage 100 100 (case player of
  X -> let xline = filled black (rect 5 64) in [ rotate (degrees 45) xline, rotate (degrees 135) xline ]
  O -> [ filled black <| circle 30,  filled white <| circle 25 ])

positionedFigure (Field col row, player) = toForm (figureElement player) |> move  (toFloat <| 100*col-200,toFloat <| -100*row+200)

boardFigures state = collage 300 300 (map positionedFigure <| board state)

-- main

main = lift view gameState
