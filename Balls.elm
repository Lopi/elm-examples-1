
import Color
import open Time
import Mouse
import Random (range)

data Position = Position Int Int
data BallSpec = BallSpec Int Int Int Color Time
data Ball = Ball Position BallSpec
makeBallSpec radius (xv,yv) color time = BallSpec radius xv yv color time
makeBall pos spec = Ball (Position (fst pos) (snd pos)) spec

computeCoordinate initialPosition length velocity time =
  let
    distance = initialPosition + round(velocity * time / 1000)
    distanceMod = distance `mod` length
    distanceDiv = distance `div` length
  in if (distanceDiv `mod` 2 == 0) then distanceMod else length - distanceMod

mapBall w h time ball =
  let
    (Ball (Position initX initY) ballSpec) = ball
    (BallSpec radius vx vy _ creationTime) = ballSpec
    relativeTime = time - creationTime
    lengthX = w - radius*2
    lengthY = h - radius*2
    x = radius + computeCoordinate (initX-radius) lengthX (toFloat vx) relativeTime
    y = radius + computeCoordinate (initY-radius) lengthY (toFloat vy) relativeTime
  in Ball (Position x y) ballSpec

mapBalls w h time balls = map (mapBall w h time) balls

-- signals

clockSignal = lift fst (timestamp (fps 50))
clickPositionsSignal = sampleOn Mouse.clicks Mouse.position
inBoxClickPositionsSignal w h =
  let positionInBox pos = fst pos <= w && snd pos <= h
  in keepIf positionInBox (0,0) clickPositionsSignal
radiusSignal w h = range 10 30 (inBoxClickPositionsSignal w h)
oneDirectionVelocitySignal w h = range 10 50 (inBoxClickPositionsSignal w h)
velocitySignal w h = lift2 (,) (oneDirectionVelocitySignal w h) (oneDirectionVelocitySignal w h)
colorSignal w h =
  let
    redSignal = range 0 220 (inBoxClickPositionsSignal w h)
    greenSignal = range 0 220 (inBoxClickPositionsSignal w h)
    blueSignal = range 0 220 (inBoxClickPositionsSignal w h)
  in lift3 rgb redSignal greenSignal blueSignal
creationTimeSignal w h = sampleOn (inBoxClickPositionsSignal w h) clockSignal
newBallSpecSignal w h = lift4 makeBallSpec (radiusSignal w h) (velocitySignal w h) (colorSignal w h) (creationTimeSignal w h)
newBallSignal w h = lift2 makeBall (inBoxClickPositionsSignal w h) (newBallSpecSignal w h)
allBallsSpecSignal w h = foldp (::) [] (newBallSignal w h)
ballsSignal w h = lift2 (mapBalls w h) clockSignal (allBallsSpecSignal w h)

-- view

box w h = collage w h [
    outlined (solid black) <| rect (toFloat w) (toFloat h),
    outlined (solid black) <| rect (toFloat (w-2)) (toFloat (h-2))
  ]

drawBall w h (Ball (Position x y) (BallSpec radius _ _ color _)) =
  filled color (circle (toFloat radius)) |> move (toFloat x,toFloat y)
                                         |> move (-(toFloat w)/2,-(toFloat h)/2)

drawBalls w h balls = collage w h (map (drawBall w h) balls)

view w h balls =
  flow down [
    layers [ box w h, drawBalls w h balls ],
    plainText "Click inside the square" --, asText balls
   ]

-- main

main' w h = lift (view w h) (ballsSignal w h)
main = main' 400 400
