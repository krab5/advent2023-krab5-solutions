{-# LANGUAGE UndecidableInstances #-}
module SGR where

data Color =
    Color3 Int | Color8 Int | Color24 (Int,Int,Int)
    deriving Show

toSGR :: Color -> String
toSGR (Color3 n) = show n
toSGR (Color8 n) = "8;5;" ++ show n
toSGR (Color24 (r,g,b)) = "8;2;" ++ show r ++ ";" ++ show g ++ ";" ++ show b

data SGR =
    Reset
  | Bold | Dim
  | Italic | Underline | StrikeOut
  | Invert
  | AltFont Int
  | Foreground Color | Background Color
  deriving Show

setSGR :: SGR -> String
setSGR Reset = "0"
setSGR Bold = "1"
setSGR Dim = "2"
setSGR Italic = "3"
setSGR Underline = "4"
setSGR StrikeOut = "9"
setSGR Invert = "7"
setSGR (AltFont i) = show (10 + i)
setSGR (Foreground c) = "3" ++ toSGR c
setSGR (Background c) = "4" ++ toSGR c

unsetSGR :: SGR -> String
unsetSGR Reset = "0"
unsetSGR Bold = "22"
unsetSGR Dim = "22"
unsetSGR Italic = "23"
unsetSGR Underline = "24"
unsetSGR StrikeOut = "29"
unsetSGR Invert = "27"
unsetSGR (AltFont _) = "10"
unsetSGR (Foreground _) = "39"
unsetSGR (Background _) = "49"

codeSGR :: String -> String
codeSGR x = "\ESC[" ++ x ++ "m"

delimit :: SGR -> String -> String
delimit s x =
    codeSGR (setSGR s) ++ x ++ codeSGR (unsetSGR s)

style :: [SGR] -> String -> String
style ss x = foldr delimit x ss

apply :: [SGR] -> String -> String
apply ss x = (concat $ map (codeSGR . setSGR) ss) ++ x


class SGRColor a where
    getColor :: a -> Color

instance Integral a => SGRColor (a,a,a) where
    getColor (r,g,b) = 
        Color24 (reg r, reg g, reg b)
        where reg = fromInteger . toInteger

instance Integral a => SGRColor a where
    getColor = Color8 . fromInteger . toInteger

instance SGRColor String where
    getColor "black"   = Color3 0
    getColor "red"     = Color3 1
    getColor "green"   = Color3 2
    getColor "yellow"  = Color3 3
    getColor "blue"    = Color3 4
    getColor "magenta" = Color3 5
    getColor "cyan"    = Color3 6
    getColor "white"   = Color3 7

fg :: SGRColor c => c -> [SGR]
fg = pure . Foreground . getColor

bg :: SGRColor c => c -> [SGR]
bg = pure . Background . getColor

bold :: [SGR]
bold = [Bold]

dim :: [SGR]
dim = [Dim]

italic :: [SGR]
italic = [Italic]

uline :: [SGR]
uline = [Underline]

sout :: [SGR]
sout = [StrikeOut]

invert :: [SGR]
invert = [Invert]

altFont :: Int -> [SGR]
altFont = return . AltFont

rst :: [SGR]
rst = [Reset]



