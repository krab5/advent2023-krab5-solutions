module TermColor where

-- | A (simple) terminal color specification, containing a foreground and a background color (both
-- optional).
data TermColor = TermColor { foreground :: Maybe Int, background :: Maybe Int }

-- | Make a term color with the provided background (and no foreground).
mkBackground :: Int -> TermColor
mkBackground n = TermColor { foreground = Nothing, background = Just n }

-- | Make a term color with provided foreground (and no background).
mkForeground :: Int -> TermColor
mkForeground n = TermColor { foreground = Just n, background = Nothing }

-- | Style the given string using the given color, using ANSI escape codes.
style :: TermColor -> String -> String
style tc s =
    mkColor "38" (foreground tc) ++ mkColor "48" (background tc) ++ s ++ mkEnd (foreground tc) (background tc)
    where mkColor _ Nothing = ""
          mkColor c (Just n) = "\ESC[" ++ c ++ ";5;" ++ show n ++ "m"
          mkEnd Nothing Nothing = ""
          mkEnd _ _ = "\ESC[0m"


