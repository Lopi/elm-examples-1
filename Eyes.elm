-- Copyright Grzegorz Balcerek 2013 www.grzegorzbalcerek.net
-- elm -r elm-runtime.js Eyes.elm

import Mouse
import Window

main = lift2 eyes Window.dimensions Mouse.position

{-
rx,ry - x and y radius of the eye
w,h - width/height of the area where the mouse pointer can move
x,y - mouse pointer coordinates
returns the coordinates of where the pupil
-}
calculatexy (rx,ry) (w,h) (x,y) =
  let tga = y / x
      xe = h/tga
      xf = w
      xef = min xe xf
      xb = (rx*ry) / (sqrt (ry^2+rx^2*tga^2))
      xa = x*xb/xef
      ye = w*tga
      yf = h
      yef = min ye yf
      yb = (rx*ry) / (sqrt (rx^2+(ry/tga)^2))
      ya = y*yb/yef
  in (xa,ya)

calculatexy' (rx,ry) (w,h) (x,y) (fx,fy) =
  let (resultx,resulty) = calculatexy (rx,ry) (w,h) (x,y) in (fx resultx,fy resulty)

{-
w,h - width/height of the screen
x,y - mouse pointer coordinates
returns the coordinates of where the pupil
-}
eyes (w,h) (x,y) =
  let fw = toFloat w
      fh = toFloat h
      fx = (toFloat x) - (fw/2)
      fy = (toFloat (h-y)) - (fh/2)
      rx = 9*fw/40
      ry = 9*fh/20
      sign x = x / (abs x)
      (rightPupilX,rightPupilY) =
        if fx >= fw/4
        then calculatexy' (rx,ry) (fw/4,fh/2) (fx-fw/4,abs fy) (\x -> x * sign (fx-fw/4) + fw/4, \y -> y * sign fy)
        else calculatexy' (rx,ry) (3*fw/4,fh/2) (fw/4-fx,abs fy) (\x -> fw/4 - x * sign (fw/4-fx), \y -> y * sign fy)
      (leftPupilX,leftPupilY) =
        if fx >= -fw/4
        then calculatexy' (rx,ry) (3*fw/4,fh/2) (fx+fw/4,abs fy) (\x -> x * sign (fx+fw/4) - fw/4, \y -> y * sign fy)
        else calculatexy' (rx,ry) (fw/4,fh/2) (-fx-fw/4,abs fy) (\x -> -x * sign (-fx-fw/4) - fw/4, \y -> y * sign fy)
      eyeBorder fw fh =
        let fw9 = (9*fw) / 10
            fh9 = (9*fh) / 10
        in group [ filled black <| oval fw fh, filled white <| oval fw9 fh9 ]
      eyeDot fw fh = filled black <| oval fw fh
  in
    collage w h
      [ eyeBorder (fw/2) fh |> moveX (-fw/4)
      , eyeBorder (fw/2) fh |> moveX (fw/4)
      , eyeDot (fw/20) (fh/10) |> move (leftPupilX,leftPupilY)
      , eyeDot (fw/20) (fh/10) |> move (rightPupilX,rightPupilY) ]
