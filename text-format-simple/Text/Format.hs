module Text.Format (
      format
    ) where

import Data.List (foldl')

{- |
Formats input string, using C\#-style.

First param is the input string in the form: \"Please, replace here {0} and here {1}\".

Second param is list of strings to put into {0}, {1} .. {N} positions.

Example: 

> format "Some {0} believes that 1 + 1 = {1}." ["people",show 10]

Result is:

> "Some people believes that 1 + 1 = 10."
-}
format :: String -> [String] -> String
format pattern = snd . foldl' replace (0, pattern)
    where
    replace :: (Int,String) -> String -> (Int,String)
    replace (num,result) str = (num + 1, replaceSubstring (getFrom num) str result)
        where
        getFrom :: Int -> String
        getFrom n = "{" ++ (show n) ++ "}"

-- Replaces all occurences rFrom to rTo in rWhere.
replaceSubstring :: String -> String -> String -> String
replaceSubstring rFrom rTo rWhere = rss $ breakSubstring rFrom rWhere
    where
    rss (before,after)
        | null after = before
        | otherwise = before ++ rTo ++
            (rss $ breakSubstring rFrom $ drop (length rFrom) after)

breakSubstring :: String -> String -> (String,String)
breakSubstring i_from i_str = doBreak i_from i_str ("","")
    where
    doBreak [] st (a,b) = (a, b ++ st)
    doBreak _ [] res = res
    doBreak (fx:fxs) (sx:sxs) (a,b) 
        | fx == sx = doBreak fxs sxs (a, b ++ [sx])
        | otherwise = doBreak i_from sxs (a ++ b ++ [sx], "")
