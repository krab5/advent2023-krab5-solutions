module Geometry.Rect where

import Text.Printf


infixl 6 |+|

(|+|) :: Num a => (a, a) -> (a, a) -> (a, a)
(x,y) |+| (x',y') = (x + x', y + y')

data Rect a = Rect { startx :: a, starty :: a, width :: a, height :: a }

inside :: (Num a, Ord a) => (a,a) -> Rect a -> Bool
inside (x,y) r = 
    (x >= startx r) && (x < startx r + width r)
    && (y >= starty r) && (y < starty r + height r)

mkRect :: (a, a) -> (a, a) -> Rect a
mkRect (x,y) (w,h) = Rect { startx = x, starty = y, width = w, height = h }

fromCorners :: Num a => (a, a) -> (a, a) -> Rect a
fromCorners (tlx,tly) (brx,bry) =
    mkRect (tlx,tly) (brx - tlx, bry - tly)

set_x :: Rect a -> a -> Rect a
set_x r x = r { startx = x }

set_y :: Rect a -> a -> Rect a
set_y r y = r { starty = y }

set_width :: Rect a -> a -> Rect a
set_width r w = r { width = w }

set_height :: Rect a -> a -> Rect a
set_height r h = r { height = h }

instance (Show a, Num a) => Show (Rect a) where
  show r =
      printf "[%s-%s]×[%s-%s]"
          (show $ startx r) (show $ startx r + width r)
          (show $ starty r) (show $ starty r + height r)

class RectLike r where
  toRect :: r a -> Rect a
  fromRect :: Rect a -> r a
  start :: r a -> (a, a)
  size :: r a -> (a, a)
  set_start :: r a -> (a, a) -> r a
  set_size :: r a -> (a, a) -> r a

top_left :: (RectLike r, Num a) => r a -> (a, a)
top_left r = start r

top_right :: (RectLike r, Num a) => r a -> (a, a)
top_right r =
    let (x,y) = start r
        (w,_) = size r in
        (x + w, y)

bottom_left :: (RectLike r, Num a) => r a -> (a, a)
bottom_left r =
    let (x,y) = start r
        (_,h) = size r in
        (x, y + h)

bottom_right :: (RectLike r, Num a) => r a -> (a, a)
bottom_right r = start r |+| size r

top_left' :: (RectLike r, Num a) => r a -> (a, a)
top_left' = bottom_left

top_right' :: (RectLike r, Num a) => r a -> (a, a)
top_right' = bottom_right

bottom_left' :: (RectLike r, Num a) => r a -> (a, a)
bottom_left' = top_left

bottom_right' :: (RectLike r, Num a) => r a -> (a, a)
bottom_right' = bottom_right

clampTo :: (Num a, Ord a) => Rect a -> Rect a -> Maybe (Rect a)
clampTo r rin =
    let (r_tlx,r_tly) = top_left r
        (r_brx,r_bry) = bottom_right r
        (rin_tlx,rin_tly) = top_left rin
        (rin_brx,rin_bry) = bottom_right rin
      in let result = fromCorners (max r_tlx rin_tlx, max r_tly rin_tly) (min r_brx rin_brx, min r_bry rin_bry)
           in if width result < fromInteger 0 || height result < fromInteger 0 then Nothing else Just result

instance RectLike Rect where
  toRect = id
  fromRect = id
  start r = (startx r, starty r)
  size r = (width r, height r)
  set_start r (x,y) = r { startx = x, starty = y }
  set_size r (w,h) = r { width = w, height = h }

intersect :: (Ord a, Num a, RectLike r) => r a -> r a -> Bool
intersect rect1 rect2 =
    not (r2 < l1 || r1 < l2 || b2 < t1 || b1 < t2)
    where (l1,t1) = top_left rect1
          (l2,t2) = top_left rect2
          (r1,b1) = bottom_right rect1
          (r2,b2) = bottom_right rect2

intersect' :: (Ord a, Num a, RectLike r) => r a -> r a -> Bool
intersect' rect1 rect2 =
    not (r2 < l1 || r1 < l2 || b2 > t1 || b1 > t2)
    where (l1,t1) = top_left' rect1
          (l2,t2) = top_left' rect2
          (r1,b1) = bottom_right' rect1
          (r2,b2) = bottom_right' rect2



